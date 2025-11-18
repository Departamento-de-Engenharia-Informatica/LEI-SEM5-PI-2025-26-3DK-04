import * as THREE from 'three';

export class WarehouseBuilder {

  /**
   * Cria um modelo de armazém industrial 3D com telhado de duas águas, portas e janelas.
   * @param width Largura do armazém (eixo X).
   * @param depth Profundidade do armazém (eixo Z).
   * @param position Posição central do armazém.
   * @param id ID para nomear o grupo.
   * @returns THREE.Group contendo o modelo do armazém.
   */
  static createWarehouse(width: number, depth: number, position: THREE.Vector3, id: number): THREE.Group {

    const group = new THREE.Group();

    // ---- Alturas e Dimensões ----
    const wallHeight = 6;
    const roofSlopeHeight = 1.5; // Altura da crista do telhado
    const eaveOverlap = 0.5;    // Extensão do beiral para fora
    const numWindows = 3;       // Número de janelas por lado

    // ---- Materiais ----
    const wallMat = new THREE.MeshStandardMaterial({
      color: 0x999999, // Cinza Concreto
      roughness: 0.8,
      metalness: 0.1
    });

    const roofMat = new THREE.MeshStandardMaterial({
      color: 0x444444, // Metal Escuro do Telhado
      roughness: 0.6,
      metalness: 0.5
    });

    const doorMat = new THREE.MeshStandardMaterial({
      color: 0x333333, // Porta Industrial Escura
      roughness: 0.7,
      metalness: 0.3
    });

    const windowMat = new THREE.MeshStandardMaterial({
      color: 0x87ceeb, // Vidro Claro (azul céu)
      transparent: true,
      opacity: 0.7,
      roughness: 0.1
    });

    // -----------------------------------------------------------------------
    // ## Corpo Principal (Paredes)
    // -----------------------------------------------------------------------
    const wallsGeom = new THREE.BoxGeometry(width, wallHeight, depth);
    const walls = new THREE.Mesh(wallsGeom, wallMat);
    walls.position.y = wallHeight / 2;
    group.add(walls);

    // -----------------------------------------------------------------------
    // ## Telhado Industrial de Duas Águas
    // -----------------------------------------------------------------------
    // Geometria para a parte superior inclinada (usando a crista no topo)
    const roofGeom = new THREE.BufferGeometry();
    const halfWidth = width / 2;
    const roofBaseY = wallHeight;
    const roofPeakY = wallHeight + roofSlopeHeight;

    // A crista do telhado deve ter o mesmo comprimento que a profundidade (depth) do armazém
    // As coordenadas XYZ definem 4 pontos em cada lado do telhado
    const vertices = new Float32Array([
      // Lado Esquerdo (com sobreposição de beiral)
      -halfWidth - eaveOverlap, roofBaseY, -depth / 2 - eaveOverlap, // 0 - Canto frontal inferior
      -halfWidth - eaveOverlap, roofBaseY, depth / 2 + eaveOverlap,  // 1 - Canto traseiro inferior
      0, roofPeakY, -depth / 2 - eaveOverlap,                        // 2 - Crista frontal
      0, roofPeakY, depth / 2 + eaveOverlap,                         // 3 - Crista traseira

      // Lado Direito (com sobreposição de beiral)
      halfWidth + eaveOverlap, roofBaseY, depth / 2 + eaveOverlap,   // 4 - Canto traseiro inferior
      halfWidth + eaveOverlap, roofBaseY, -depth / 2 - eaveOverlap,  // 5 - Canto frontal inferior
      0, roofPeakY, depth / 2 + eaveOverlap,                         // 6 - Crista traseira (duplicado do 3, mas necessário para as faces)
      0, roofPeakY, -depth / 2 - eaveOverlap,                        // 7 - Crista frontal (duplicado do 2)
    ]);

    // Definição dos triângulos para os dois lados do telhado
    const indices = [
      // Lado Esquerdo
      0, 1, 3, // Triângulo 1
      0, 3, 2, // Triângulo 2
      // Lado Direito
      5, 4, 6, // Triângulo 3
      5, 6, 7  // Triângulo 4
    ];

    roofGeom.setIndex(indices);
    roofGeom.setAttribute('position', new THREE.BufferAttribute(vertices, 3));
    roofGeom.computeVertexNormals(); // Calcular normais para iluminação correta

    const roof = new THREE.Mesh(roofGeom, roofMat);
    group.add(roof);

    // -----------------------------------------------------------------------
    // ## Porta Industrial Frontal
    // -----------------------------------------------------------------------
    const doorWidth = width * 0.45;
    const doorHeight = 4.5;
    const doorGeom = new THREE.BoxGeometry(doorWidth, doorHeight, 0.2);
    const door = new THREE.Mesh(doorGeom, doorMat);
    // Posição: centro (X), metade da altura (Y), na face frontal (Z)
    door.position.set(0, doorHeight / 2, -(depth / 2 + 0.11));
    group.add(door);

    // Moldura Simples (opcional, pode ser mais detalhada)
    const frameGeom = new THREE.BoxGeometry(doorWidth + 0.1, doorHeight + 0.1, 0.1);
    const frameMat = new THREE.MeshStandardMaterial({ color: 0x555555 });
    const frame = new THREE.Mesh(frameGeom, frameMat);
    frame.position.set(0, doorHeight / 2, depth / 2 + 0.16); // Ligeiramente à frente da porta
    group.add(frame);


    // -----------------------------------------------------------------------
    // ## Janelas Laterais (Apenas 1 na demonstração para brevidade, mas o loop pode criar várias)
    // -----------------------------------------------------------------------
    const windowWidth = 2;
    const windowHeight = 1;
    const windowGeom = new THREE.BoxGeometry(windowWidth, windowHeight, 0.1);
    const windowXOffset = (width / (numWindows + 1));
    const windowYPos = wallHeight - (windowHeight / 2) - 0.5; // Posição alta

    for (let i = 0; i < numWindows; i++) {
      const xPos = -width / 2 + windowXOffset * (i + 1);

      // Parede Esquerda (-X)
      const windowLeft = new THREE.Mesh(windowGeom, windowMat);
      windowLeft.rotation.y = Math.PI / 2; // Gira para alinhar com o eixo Z
      windowLeft.position.set(-width / 2 - 0.1, windowYPos, xPos);
      group.add(windowLeft);

      // Parede Direita (+X)
      const windowRight = new THREE.Mesh(windowGeom, windowMat);
      windowRight.rotation.y = Math.PI / 2;
      windowRight.position.set(width / 2 + 0.1, windowYPos, xPos);
      group.add(windowRight);
    }
    // -----------------------------------------------------------------------

    // ---- Posicionamento Final ----
    group.position.copy(position);
    group.position.y = 0; // Garante que a base do armazém esteja em y=0

    group.name = `Warehouse_${id}`;

    return group;
  }
}
