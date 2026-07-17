## 7\. Visualization: OpenGL/Vulkan The user interface requires a real-time, 3D wireframe display of the patient's breathing surface to allow the therapist to visually verify alignment.

### 7.1 Performance Constraints: \`Gloss\` vs. \`OpenGLRaw\` While the Haskell library \`Gloss\` is excellent for 2D visualizations, it is ill-suited for this application. \`Gloss\` typically rebuilds the entire scene graph every frame and lacks efficient mechanisms for updating dynamic 3D geometry.\[19, 20\] Updating 400 vertices at 20-30 FPS requires \*\*Vertex Buffer Objects (VBOs)\*\*.

### 7.2 Efficient Mesh Rendering with \`OpenGLRaw\` We utilize \*\*\`Graphics.Rendering.OpenGL.Raw\`\*\* for direct access to the GPU pipeline. 1\. \*\*Initialization\*\*: A static VBO is allocated for the grid indices (connectivity), and a dynamic VBO is allocated for the vertex positions. 2\. \*\*Per-Frame Update\*\*: \* The CPU calculates the 400 $(x,y,z)$ coordinates of the virtual mesh (from Layer 3). \* We use \`glBufferSubData\` (or \`glMapBuffer\`) to upload this small dataset (\~5 KB) to the GPU. This avoids the overhead of immediate mode (\`glBegin/glEnd\`).\[21\] 3\. \*\*Rendering\*\*: \* We set \`glPolygonMode(GL\_FRONT\_AND\_BACK, GL\_LINE)\` to render a wireframe.\[22\] This allows the therapist to see "through" the surface to the target isocenter. \* A Fragment Shader colors the mesh \*\*Green\*\* if the Gating Logic is "Beam On" and \*\*Red\*\* if "Beam Off," providing immediate visual feedback. \---


## Explicit Software Unit Interfaces and Failure Boundaries

### FR-UI-001: Real-time visualization
- **Module:** `cbits/src/hud.cpp`, `cbits/include/hud.h`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Real-time visualization, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### FR-UI-002: Visual gating feedback
- **Module:** `cbits/src/hud.cpp`, `cbits/include/hud.h`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Visual gating feedback, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### FR-UI-003: Structural navigation, assistive technology hooks, and skip-links
- **Module:** `cbits/src/hud.cpp`, `cbits/include/hud.h`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Structural navigation, assistive technology hooks, and skip-links, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### SR-UI-001: Multi-modal encoding (color, shape, and symbol) for safety-critical states
- **Module:** `app/Control/UI/Renderer.hs`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Multi-modal encoding (color, shape, and symbol) for safety-critical states, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### SR-UI-002: Formal usability verification checklist for visual inspections
- **Module:** `docs/qms/usability_checklist.md`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Formal usability verification checklist for visual inspections, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.
