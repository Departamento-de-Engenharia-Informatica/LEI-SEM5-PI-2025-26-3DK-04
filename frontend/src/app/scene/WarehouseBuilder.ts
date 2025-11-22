import * as THREE from 'three';

export class WarehouseBuilder {
  private static textureLoader = new THREE.TextureLoader();

  static createWarehouse(width: number, depth: number, position: THREE.Vector3, id: string): THREE.Group {
    const group = new THREE.Group();

    // ---- Alturas e Dimensões ----
    const wallHeight = 6;
    const roofSlopeHeight = 1.5;
    const eaveOverlap = 0.5;
    const numWindows = 3;

    // ---- Materiais com texturas internas ----
    const wallMat = new THREE.MeshStandardMaterial({
      map: this.textureLoader.load('/assets/textures/warehouse/wall.jpg'),
      color: 0xffffff,
      roughness: 0.8,
      metalness: 0.1
    });

    const roofMat = new THREE.MeshStandardMaterial({
      map: this.textureLoader.load('/assets/textures/warehouse/roof.jpg'),
      color: 0xffffff,
      roughness: 0.6,
      metalness: 0.5,
      side: THREE.DoubleSide
    });

    const gableMat = new THREE.MeshStandardMaterial({
      map: this.textureLoader.load('/assets/textures/warehouse/gable.jpg'),
      color: 0xffffff,
      roughness: 0.8,
      metalness: 0.1,
      side: THREE.DoubleSide
    });

    const frameMat = new THREE.MeshStandardMaterial({
      color: 0x4f4f4f,
      roughness: 0.8
    });

    const doorMat = new THREE.MeshStandardMaterial({
      map: this.textureLoader.load('/assets/textures/warehouse/door.jpg'),
      color: 0xffffff,
      roughness: 0.7,
      metalness: 0.3
    });

    const windowMat = new THREE.MeshStandardMaterial({
      map: this.textureLoader.load('/assets/textures/warehouse/window.jpg'),
      color: 0xffffff,
      transparent: true,
      opacity: 0.7,
      roughness: 0.1
    });

    // ---- Paredes ----
    const wallsGeom = new THREE.BoxGeometry(width, wallHeight, depth);
    const walls = new THREE.Mesh(wallsGeom, wallMat);
    walls.position.y = wallHeight / 2;
    group.add(walls);

    const halfWidth = width / 2;
    const halfDepth = depth / 2;
    const roofBaseY = wallHeight;
    const roofPeakY = wallHeight + roofSlopeHeight;

    // ---- Telhado ----
    // Lado esquerdo do telhado
    const leftRoofGeom = new THREE.BufferGeometry();
    const leftVertices = new Float32Array([
      -halfWidth - eaveOverlap, roofBaseY, -halfDepth - eaveOverlap,
      -halfWidth - eaveOverlap, roofBaseY,  halfDepth + eaveOverlap,
      0, roofPeakY, -halfDepth - eaveOverlap,
      0, roofPeakY,  halfDepth + eaveOverlap
    ]);
    leftRoofGeom.setIndex([0,1,3,0,3,2]);
    leftRoofGeom.setAttribute('position', new THREE.BufferAttribute(leftVertices, 3));
    leftRoofGeom.computeVertexNormals();

// UVs
    leftRoofGeom.computeBoundingBox();
    const leftBBox = leftRoofGeom.boundingBox!;
    const leftUV: number[] = [];
    for (let i = 0; i < leftVertices.length; i += 3) {
      const x = leftVertices[i];
      const z = leftVertices[i + 2];
      const u = (x - leftBBox.min.x) / (leftBBox.max.x - leftBBox.min.x);
      const v = (z - leftBBox.min.z) / (leftBBox.max.z - leftBBox.min.z);
      leftUV.push(u, v);
    }
    leftRoofGeom.setAttribute('uv', new THREE.Float32BufferAttribute(leftUV, 2));

    const leftRoof = new THREE.Mesh(leftRoofGeom, roofMat);
    group.add(leftRoof);

// Lado direito do telhado
    const rightRoofGeom = new THREE.BufferGeometry();
    const rightVertices = new Float32Array([
      halfWidth + eaveOverlap, roofBaseY, -halfDepth - eaveOverlap,
      halfWidth + eaveOverlap, roofBaseY,  halfDepth + eaveOverlap,
      0, roofPeakY, -halfDepth - eaveOverlap,
      0, roofPeakY,  halfDepth + eaveOverlap
    ]);
    rightRoofGeom.setIndex([0,2,3,0,3,1]);
    rightRoofGeom.setAttribute('position', new THREE.BufferAttribute(rightVertices, 3));
    rightRoofGeom.computeVertexNormals();

// UVs
    rightRoofGeom.computeBoundingBox();
    const rightBBox = rightRoofGeom.boundingBox!;
    const rightUV: number[] = [];
    for (let i = 0; i < rightVertices.length; i += 3) {
      const x = rightVertices[i];
      const z = rightVertices[i + 2];
      const u = (x - rightBBox.min.x) / (rightBBox.max.x - rightBBox.min.x);
      const v = (z - rightBBox.min.z) / (rightBBox.max.z - rightBBox.min.z);
      rightUV.push(u, v);
    }
    rightRoofGeom.setAttribute('uv', new THREE.Float32BufferAttribute(rightUV, 2));

    const rightRoof = new THREE.Mesh(rightRoofGeom, roofMat);
    group.add(rightRoof);


    // ---- Gable Walls ----
    const createGable = (z: number) => {
      const geom = new THREE.BufferGeometry();
      const verts = new Float32Array([
        -halfWidth, roofBaseY, z,
        halfWidth, roofBaseY, z,
        0, roofPeakY, z
      ]);
      geom.setAttribute("position", new THREE.BufferAttribute(verts, 3));
      geom.setIndex([0,1,2]);
      geom.computeVertexNormals();
      return new THREE.Mesh(geom, gableMat);
    };
    group.add(createGable(-halfDepth)); // frente
    group.add(createGable(halfDepth));  // traseira

    // ---- Porta industrial ----
    const doorWidth = width * 0.45;
    const doorHeight = 4.5;
    const door = new THREE.Mesh(new THREE.BoxGeometry(doorWidth, doorHeight, 0.2), doorMat);
    door.position.set(0, doorHeight / 2, -halfDepth - 0.11);
    group.add(door);

    const frame = new THREE.Mesh(new THREE.BoxGeometry(doorWidth + 0.25, doorHeight + 0.25, 0.15), frameMat);
    frame.position.set(0, doorHeight / 2, -halfDepth - 0.2);
    group.add(frame);

    const ribGeom = new THREE.BoxGeometry(0.05, doorHeight * 0.9, 0.25);
    const ribCount = 6;
    const ribSpacing = doorWidth / (ribCount + 1);
    for (let i = 0; i < ribCount; i++) {
      const rib = new THREE.Mesh(ribGeom, doorMat);
      rib.position.set(-doorWidth / 2 + ribSpacing * (i + 1), doorHeight / 2, -halfDepth - 0.25);
      group.add(rib);
    }

    // ---- Janelas laterais proporcionais e dentro da parede ----
    const windowHeight = wallHeight / 3;
    const windowWidth = width / 8;
    const windowDepth = 0.1;

    const windowGeom = new THREE.BoxGeometry(windowDepth, windowHeight, windowWidth); // largura ao longo Z
    const windowYPos = wallHeight / 2 + windowHeight / 2; // centro vertical da parede
    const windowZOffset = width / (numWindows + 1);

    for (let i = 0; i < numWindows; i++) {
      const zPos = -width / 2 + windowZOffset * (i + 1);

      // Esquerda (x negativo)
      const windowLeft = new THREE.Mesh(windowGeom, windowMat);
      windowLeft.position.set(-halfWidth -0.1, windowYPos - 0.67, zPos);
      windowLeft.rotation.x = Math.PI / 2; // gira para encaixar na parede
      group.add(windowLeft);

      // Direita (x positivo)
      const windowRight = new THREE.Mesh(windowGeom, windowMat);
      windowRight.position.set(halfWidth +0.1, windowYPos - 0.67, zPos);
      windowRight.rotation.x = Math.PI / 2;
      group.add(windowRight);
    }


    // ---- Posicionamento Final ----
    group.position.copy(position);
    group.position.y = 0;
    group.name = `Warehouse_${id}`;
    group.traverse(obj => {
      if (obj instanceof THREE.Mesh) {
        obj.castShadow = true;    // projeta sombra
        obj.receiveShadow = true; // recebe sombra
      }
    });
    return group;
  }
}
