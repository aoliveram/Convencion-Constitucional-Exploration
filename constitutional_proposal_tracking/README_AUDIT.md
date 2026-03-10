# Auditoría de Calidad de Datos: Indicaciones

Este branch ha sido creado para abordar el bloqueo crítico identificado en el proceso de reconstrucción de borradores constitucionales.

## El Problema
Contamos con un motor de reconstrucción basado en IA (`06_apply_indications_ai_v3.py`) que es capaz de generar el historial evolutivo de los artículos. Sin embargo, los datos de entrada en la carpeta `indicaciones-universal-extracted/` presentan inconsistencias graves:

1. **Ambigüedad en el `target_article`**: Muchas indicaciones apuntan genéricamente al "Artículo 1" cuando deberían referirse a sub-secciones específicas (ej: "Artículo 1, número 3").
2. **Falta de Contexto**: La IA no puede distinguir correctamente entre múltiples referencias con el mismo número si la extracción del PDF no fue ultra-precisa.
3. **Consecuencia**: Correr el motor sobre estos datos genera borradores corruptos, eliminaciones erróneas y pérdida de trazabilidad.

## Objetivo del Colaborador
Realizar una revisión manual exhaustiva de los archivos JSON de indicaciones para:
- Validar que el `target_article` coincida con lo que dice el PDF original de votación.
- Corregir el contenido de las indicaciones si la extracción fue incompleta.
- Asegurar que la estructura del JSON permita al motor V3 aplicar los cambios sin ambigüedad.

## Próximos Pasos
Una vez corregidos los JSON en este branch, podremos reiniciar el motor de reconstrucción para Comisiones 1 a 7.
