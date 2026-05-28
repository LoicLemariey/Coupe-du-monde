import asyncio
import json
import re
from datetime import datetime
from playwright.async_api import async_playwright


async def scrape_winamax_coupe_du_monde():
    """
    Scrape les cotes des scores exacts de la Coupe du Monde 2026 sur Winamax
    """
    competition_id = 900001750
    url = f"https://www.winamax.fr/paris-sportifs/sports/1/4/{competition_id}"
    
    matches_data = []
    all_requests = []
    
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False)  # Mode non-headless pour voir ce qui se passe
        context = await browser.new_context(
            user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
        )
        page = await context.new_page()
        
        # Intercepter TOUS les événements réseau
        def handle_request(request):
            all_requests.append({
                'method': request.method,
                'url': request.url,
                'type': request.resource_type
            })
            if 'api' in request.url or 'graphql' in request.url or '.json' in request.url:
                print(f"📤 Requête: {request.url[:100]}")
        
        def handle_response(response):
            try:
                if any(x in response.url for x in ['api', 'graphql', 'events', '.json', 'odds']):
                    print(f"📥 Réponse: {response.url[:100]} - Status: {response.status}")
            except:
                pass
        
        page.on("request", handle_request)
        page.on("response", handle_response)
        
        try:
            print(f"📡 Connexion à {url}...")
            await page.goto(url, wait_until="domcontentloaded", timeout=30000)
            
            # Attendre plus longtemps pour laisser les données se charger
            print("⏳ Attente du chargement des données (10 secondes)...")
            await asyncio.sleep(10)
            
            # Faire défiler pour charger plus de contenu
            print("📜 Défilement de la page...")
            await page.evaluate("window.scrollBy(0, window.innerHeight * 5)")
            await asyncio.sleep(2)
            
            # Récupérer le HTML de la page complète
            print("🔍 Extraction du contenu de la page...")
            page_content = await page.content()
            
            # Chercher les cotes et les matchs dans le HTML
            # Chercher les patterns de noms d'équipes et cotes
            score_patterns = re.findall(r'(\d+\.\d+)', page_content)
            team_patterns = re.findall(r'>([A-Z][a-z\s]+)<', page_content)
            
            print(f"\n📊 Résultats de l'extraction:")
            print(f"   Cotes trouvées: {len(set(score_patterns))}")
            print(f"   Équipes potentielles: {len(set(team_patterns))}")
            
            # Extraire les données via JavaScript
            page_data = await page.evaluate("""
                () => {
                    return {
                        title: document.title,
                        bodyHTML: document.body.innerHTML.substring(0, 5000),
                        allText: document.body.innerText.substring(0, 3000),
                        matchCount: document.querySelectorAll('[class*="event"], [class*="match"]').length
                    };
                }
            """)
            
            # Sauvegarder toutes les données
            output_file = "winamax_matches.json"
            output_data = {
                "timestamp": datetime.now().isoformat(),
                "url": url,
                "network_requests": all_requests[-20:],  # Les 20 dernières requêtes
                "page_data": page_data,
                "scores_found": list(set(score_patterns))[:20],
                "teams_found": list(set(team_patterns))[:20]
            }
            
            with open(output_file, "w", encoding="utf-8") as f:
                json.dump(output_data, f, indent=2, ensure_ascii=False)
            
            print(f"\n✅ Données sauvegardées dans {output_file}")
            print(f"📝 Fichier créé avec {len(all_requests)} requêtes réseau capturées")
            
        except Exception as e:
            print(f"❌ Erreur: {str(e)}")
            import traceback
            traceback.print_exc()
        
        finally:
            # Laisser le navigateur ouvert 5 secondes pour inspection
            print("\n💡 Le navigateur reste ouvert 5 secondes pour inspection...")
            await asyncio.sleep(5)
            await browser.close()
    
    return matches_data


# Exécuter le scraping
if __name__ == "__main__":
    print("🏆 Scraper Winamax - Coupe du Monde 2026 (v3 - Inspection réseau)")
    print("=" * 60)
    
    try:
        data = asyncio.run(scrape_winamax_coupe_du_monde())
        print(f"\n📦 Scraping terminé avec succès")
    except KeyboardInterrupt:
        print("\n⏹️ Scraping annulé par l'utilisateur")
    except Exception as e:
        print(f"\n❌ Erreur: {str(e)}")