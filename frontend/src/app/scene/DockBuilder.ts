import * as THREE from 'three';

export class DockBuilder {

  static createDock(width: number, depth: number, position: THREE.Vector3, id: string): THREE.Group {
    const group = new THREE.Group();
    const loader = new THREE.TextureLoader();

    // Alturas
    const baseHeight = 1;
    const deckThickness = 0.35;

    // ------ BASE PRINCIPAL ------
    const baseGeom = new THREE.BoxGeometry(width, baseHeight, depth);
    const baseMat = new THREE.MeshStandardMaterial({
      map: loader.load('/assets/textures/dock/base.jpg'),
      roughness: 0.8,
      metalness: 0.1
    });
    const base = new THREE.Mesh(baseGeom, baseMat);
    base.position.y = baseHeight / 2;
    group.add(base);

    // ------ DECK SUPERIOR ------
    // ------ DECK SUPERIOR COM TEXTURA REPETIDA ------
    const deckGeom = new THREE.BoxGeometry(width, deckThickness, depth);

// Carrega a textura
    const deckTexture = loader.load('/assets/textures/dock/deck.jpg');
    deckTexture.wrapS = THREE.RepeatWrapping;
    deckTexture.wrapT = THREE.RepeatWrapping;

// Ajusta quantas vezes a textura se repete
    const repeatX = Math.floor(width /4);   // repetir a cada 2 unidades de largura
    const repeatZ = Math.floor(depth / 4);   // repetir a cada 2 unidades de profundidade
    deckTexture.repeat.set(repeatX, repeatZ);

    const deckMat = new THREE.MeshStandardMaterial({
      map: deckTexture,
      roughness: 0.7,
      metalness: 0.15
    });

    const deck = new THREE.Mesh(deckGeom, deckMat);
    deck.position.y = baseHeight + deckThickness / 2;
    group.add(deck);


    // -------------------------------------------------------
    //   PILARES DE SUPORTE (FRENTE E TRÁS)
    // -------------------------------------------------------
    const pillarGeom = new THREE.BoxGeometry(0.5, 6, 0.5);
    const pillarMat = new THREE.MeshStandardMaterial({
      map: loader.load('/assets/textures/dock/pillar.jpg'),
      roughness: 0.6,
      metalness: 0.4
    });

    const pillarCount = Math.max(3, Math.floor(width / 6));
    for (let i = 0; i < pillarCount; i++) {
      const x = -width / 2 + (i * width) / (pillarCount - 1);

      const pillarFront = new THREE.Mesh(pillarGeom, pillarMat);
      pillarFront.position.set(x, baseHeight - 3, depth / 2 - 0.6);
      group.add(pillarFront);

      const pillarBack = new THREE.Mesh(pillarGeom, pillarMat);
      pillarBack.position.set(x, baseHeight - 3, -depth / 2 + 0.6);
      group.add(pillarBack);
    }

    // -------------------------------------------------------
    //                BOLLARDS (AMARAR CABOS)
    // -------------------------------------------------------
    const bollardGeom = new THREE.CylinderGeometry(0.25, 0.25, 0.6, 16);
    const bollardMat = new THREE.MeshStandardMaterial({
      map: loader.load('/assets/textures/dock/bollard.jpg'),
      roughness: 0.5,
      metalness: 0.7
    });

    const bollardCount = Math.max(3, Math.floor(width / 5));
    for (let i = 0; i < bollardCount; i++) {
      const x = -width / 2 + (i * width) / (bollardCount - 1);

      const bollard = new THREE.Mesh(bollardGeom, bollardMat);
      bollard.position.set(x, baseHeight + 0.5, -depth / 2 + 0.25);
      group.add(bollard);
    }

    // -------------------------------------------------------
    //                 ANÉIS DE AMARRAÇÃO
    // -------------------------------------------------------
    const ringGeom = new THREE.TorusGeometry(0.25, 0.08, 10, 20);
    const ringMat = new THREE.MeshStandardMaterial({
      map: loader.load('/assets/textures/dock/ring.jpg'),
      roughness: 0.4,
      metalness: 0.8
    });

    for (let i = 0; i < bollardCount; i++) {
      const x = -width / 2 + (i * width) / (bollardCount - 1);

      const ring = new THREE.Mesh(ringGeom, ringMat);
      ring.position.set(x, baseHeight + 0.25, -depth / 2 - 0.05);
      ring.rotation.x = Math.PI / 2;
      group.add(ring);
    }

    // ------ POSICIONAR ------
    group.position.copy(position);
    group.position.y = 0;
    group.name = `Dock_${id}`;

    DockBuilder.builtDocks.set(id, {
      group,
      z: position.z
    });

    return group;
  }

  static builtDocks: Map<string, { group: THREE.Group; z: number }> = new Map();
}
