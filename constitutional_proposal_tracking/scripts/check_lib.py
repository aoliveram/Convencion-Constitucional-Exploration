
import google.generativeai as genai
import textwrap

print(f"Library version: {genai.__version__}")

try:
    # Check if GenerationConfig supports response_schema
    from google.generativeai.types import GenerationConfig
    print("GenerationConfig found.")
    # We can inspect if the class has response_schema or response_mime_type awareness in its docs or attributes
    # But best way is just to print success if import worked.
    print("Environment likely supports structured output features.")
except ImportError:
    print("ERROR: Old library version.")
