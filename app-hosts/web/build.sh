#!/bin/bash

# Build script for XR-DSL Web target

set -e

echo "🔧 Building XR-DSL for WebAssembly..."

# Install wasm-pack if not available
if ! command -v wasm-pack &> /dev/null; then
    echo "📦 Installing wasm-pack..."
    curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
fi

# Build the WebAssembly module
echo "🚀 Compiling to WebAssembly..."
wasm-pack build --target web --out-dir pkg --dev

# Create a simple HTTP server script
echo "🌐 Creating development server..."
cat > serve.py << 'EOF'
#!/usr/bin/env python3
import http.server
import socketserver
import webbrowser
import os

PORT = 8000

class MyHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header('Cross-Origin-Embedder-Policy', 'require-corp')
        self.send_header('Cross-Origin-Opener-Policy', 'same-origin')
        super().end_headers()

os.chdir(os.path.dirname(os.path.abspath(__file__)))

with socketserver.TCPServer(("", PORT), MyHTTPRequestHandler) as httpd:
    print(f"🎉 XR-DSL Web Demo serving at http://localhost:{PORT}")
    print("🔄 The page will open automatically...")
    webbrowser.open(f'http://localhost:{PORT}')
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\n👋 Server stopped")
EOF

chmod +x serve.py

echo "✅ Build complete!"
echo ""
echo "🚀 To run the demo:"
echo "   cd app-hosts/web"
echo "   ./serve.py"
echo ""
echo "Or manually serve the current directory and open index.html"