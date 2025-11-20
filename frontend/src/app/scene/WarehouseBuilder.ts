import * as THREE from 'three';

export class WarehouseBuilder {

  static createWarehouse(width: number, depth: number, position: THREE.Vector3, id: number): THREE.Group {

    const group = new THREE.Group();

    // ---- Alturas e Dimensões ----
    const wallHeight = 6;
    const roofSlopeHeight = 1.5;
    const eaveOverlap = 0.5;
    const numWindows = 3;

    // ---- Materiais ----
    const wallMat = new THREE.MeshStandardMaterial({
      color: 0x999999,
      roughness: 0.8,
      metalness: 0.1
    });

    const roofMat = new THREE.MeshStandardMaterial({
      color: 0x444444,
      roughness: 0.6,
      metalness: 0.5,
      side: THREE.DoubleSide
    });

    const gableMat = new THREE.MeshStandardMaterial({
      color: 0x999999,
      roughness: 0.8,
      metalness: 0.1,
      side: THREE.DoubleSide
    });

    const frameMat = new THREE.MeshStandardMaterial({
      color: 0x4f4f4f,
      roughness: 0.8
    });

    const doorMat = new THREE.MeshStandardMaterial({
      color: 0x333333,
      roughness: 0.7,
      metalness: 0.3
    });

    const windowMat = new THREE.MeshStandardMaterial({
      color: 0x87ceeb,
      transparent: true,
      opacity: 0.7,
      roughness: 0.1
    });


    const wallsGeom = new THREE.BoxGeometry(width, wallHeight, depth);
    const walls = new THREE.Mesh(wallsGeom, wallMat);
    walls.position.y = wallHeight / 2;
    group.add(walls);


    const halfWidth = width / 2;
    const halfDepth = depth / 2;
    const roofBaseY = wallHeight;
    const roofPeakY = wallHeight + roofSlopeHeight;

    // Geometria do telhado
    const roofGeom = new THREE.BufferGeometry();

    const vertices = new Float32Array([

      -halfWidth - eaveOverlap, roofBaseY, -halfDepth - eaveOverlap,
      -halfWidth - eaveOverlap, roofBaseY,  halfDepth + eaveOverlap,
      0, roofPeakY, -halfDepth - eaveOverlap,
      0, roofPeakY,  halfDepth + eaveOverlap,

      // Lado direito
      halfWidth + eaveOverlap, roofBaseY,  halfDepth + eaveOverlap,
      halfWidth + eaveOverlap, roofBaseY, -halfDepth - eaveOverlap,
      0, roofPeakY,  halfDepth + eaveOverlap,
      0, roofPeakY, -halfDepth - eaveOverlap,
    ]);

    const indices = [
      0, 1, 3, 0, 3, 2,   // esquerda
      5, 4, 6, 5, 6, 7    // direita
    ];

    roofGeom.setIndex(indices);
    roofGeom.setAttribute('position', new THREE.BufferAttribute(vertices, 3));
    roofGeom.computeVertexNormals();

    const roof = new THREE.Mesh(roofGeom, roofMat);
    group.add(roof);

    // -----------------------------------------------------------------------
    // ## Paredes triangulares superiores (Gable Walls)
    // -----------------------------------------------------------------------

    // Frente (-Z)
    const frontGable = new THREE.Mesh(new THREE.BufferGeometry(), gableMat);
    {
      const verts = new Float32Array([
        -halfWidth, roofBaseY, -halfDepth,
        halfWidth, roofBaseY, -halfDepth,
        0,         roofPeakY, -halfDepth
      ]);
      frontGable.geometry.setAttribute("position", new THREE.BufferAttribute(verts, 3));
      frontGable.geometry.setIndex([0,1,2]);
      frontGable.geometry.computeVertexNormals();
    }
    group.add(frontGable);

    // Traseira (+Z)
    const backGable = new THREE.Mesh(new THREE.BufferGeometry(), gableMat);
    {
      const verts = new Float32Array([
        -halfWidth, roofBaseY, halfDepth,
        halfWidth, roofBaseY, halfDepth,
        0,         roofPeakY, halfDepth
      ]);
      backGable.geometry.setAttribute("position", new THREE.BufferAttribute(verts, 3));
      backGable.geometry.setIndex([0,1,2]);
      backGable.geometry.computeVertexNormals();
    }
    group.add(backGable);

    // -----------------------------------------------------------------------
    // ## Porta Industrial
    // -----------------------------------------------------------------------
    const doorWidth = width * 0.45;
    const doorHeight = 4.5;

    const door = new THREE.Mesh(
      new THREE.BoxGeometry(doorWidth, doorHeight, 0.2),
      doorMat
    );
    door.position.set(0, doorHeight / 2, -halfDepth - 0.11);
    group.add(door);

    // Moldura
    const frame = new THREE.Mesh(
      new THREE.BoxGeometry(doorWidth + 0.25, doorHeight + 0.25, 0.15),
      frameMat
    );
    frame.position.set(0, doorHeight / 2, -halfDepth - 0.2);
    group.add(frame);

    // Relevos (linhas verticais)
    const ribGeom = new THREE.BoxGeometry(0.05, doorHeight * 0.9, 0.25);
    const ribCount = 6;
    const ribSpacing = doorWidth / (ribCount + 1);

    for (let i = 0; i < ribCount; i++) {
      const rib = new THREE.Mesh(ribGeom, doorMat);
      rib.position.set(
        -doorWidth / 2 + ribSpacing * (i + 1),
        doorHeight / 2,
        -halfDepth - 0.25
      );
      group.add(rib);
    }

    // -----------------------------------------------------------------------
    // ## Janelas laterais
    // -----------------------------------------------------------------------
    const windowWidth = 2;
    const windowHeight = 1;
    const windowGeom = new THREE.BoxGeometry(windowWidth, windowHeight, 0.1);
    const windowXOffset = (width / (numWindows + 1));
    const windowYPos = wallHeight - (windowHeight / 2) - 0.5;

    for (let i = 0; i < numWindows; i++) {
      const xPos = -width / 2 + windowXOffset * (i + 1);

      // Esquerda
      const windowLeft = new THREE.Mesh(windowGeom, windowMat);
      windowLeft.rotation.y = Math.PI / 2;
      windowLeft.position.set(-halfWidth - 0.1, windowYPos, xPos);
      group.add(windowLeft);

      // Direita
      const windowRight = new THREE.Mesh(windowGeom, windowMat);
      windowRight.rotation.y = Math.PI / 2;
      windowRight.position.set(halfWidth + 0.1, windowYPos, xPos);
      group.add(windowRight);
    }

    // ---- Posicionamento Final ----
    group.position.copy(position);
    group.position.y = 0;

    group.name = `Warehouse_${id}`;

    return group;
  }
}
