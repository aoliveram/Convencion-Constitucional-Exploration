# embeddings_vs_tfidf.py
import pandas as pd
import numpy as np
from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity
import pyreadr
from tqdm import tqdm

# --- 1. Cargar datos exportados desde R ---
print("Cargando datos desde R...")
borrador_df = pyreadr.read_r('scripts - files/analizar_procedencia_borrador/oraciones_borrador_df.rds')[None]
iniciativas_df = pyreadr.read_r('oraciones_iniciativas_df.rds')[None]
top10_tfidf = pyreadr.read_r('top10_tfidf_matches.rds')[None]

# Aseguramos que estén en el mismo orden de indexación que en R
borrador_texts = borrador_df['oracion_limpia'].tolist()
iniciativas_texts = iniciativas_df['oracion_limpia'].tolist()

n_borrador = len(borrador_texts)

# --- 2. Generar embeddings ---
print("Cargando modelo de embeddings...")
model = SentenceTransformer('distiluse-base-multilingual-cased-v2')  # rápido y multilingüe

print("Generando embeddings para borrador...")
emb_borrador = model.encode(borrador_texts, batch_size=64, show_progress_bar=True)

print("Generando embeddings para iniciativas...")
emb_iniciativas = model.encode(iniciativas_texts, batch_size=64, show_progress_bar=True)

# --- 3. Calcular similitudes y top-10 ---
print("Calculando matriz de similitud (esto puede ser pesado)...")
similarity_matrix_emb = cosine_similarity(emb_borrador, emb_iniciativas)

print("Extrayendo top-10 por oración...")
top10_emb_list = []
for i in tqdm(range(n_borrador)):
    sims = similarity_matrix_emb[i]
    top_idx = np.argsort(sims)[::-1][:10]
    for rank, idx in enumerate(top_idx, start=1):
        top10_emb_list.append({
            'id_oracion_borrador': i+1,  # R indexaba desde 1
            'rank': rank,
            'id_oracion_iniciativa_match': idx+1,
            'similitud_emb': sims[idx]
        })

top10_emb = pd.DataFrame(top10_emb_list)

# --- 4. Comparar TF-IDF vs Embeddings ---
print("Comparando resultados...")

comparison_df = pd.merge(
    top10_tfidf.rename(columns={
        'id_oracion_iniciativa_match': 'id_oracion_iniciativa_match_tfidf',
        'similitud': 'similitud_tfidf'
    }),
    top10_emb.rename(columns={
        'id_oracion_iniciativa_match': 'id_oracion_iniciativa_match_emb'
    }),
    on=['id_oracion_borrador', 'rank'],
    how='inner'
)

# Porcentaje de coincidencias exactas por rank
pct_match_by_rank = (comparison_df
    .assign(match_exacto=lambda d: d['id_oracion_iniciativa_match_tfidf'] == d['id_oracion_iniciativa_match_emb'])
    .groupby('rank')['match_exacto']
    .mean() * 100
).reset_index(name='pct_match')

# Correlación de similitud en rank 1
rank1_corr = (comparison_df
    .query('rank == 1')[['similitud_tfidf', 'similitud_emb']]
    .corr().iloc[0, 1]
)

print("\n--- Porcentaje de coincidencias exactas por rank ---")
print(pct_match_by_rank)

print(f"\nCorrelación de similitud (rank 1): {rank1_corr:.4f}")

# --- 5. Guardar resultados ---
comparison_df.to_csv("comparacion_tfidf_embeddings.csv", index=False)
print("\nResultados guardados en comparacion_tfidf_embeddings.csv")