import * as THREE from 'three';

export class DockBuilder {

  static createDock(width: number, depth: number, position: THREE.Vector3, id: number): THREE.Group {

    const group = new THREE.Group();

    // Alturas
    const baseHeight = 1;
    const deckThickness = 0.3;

    // ---- Base estrutural ----
    const baseGeom = new THREE.BoxGeometry(width, baseHeight, depth);
    const baseMat = new THREE.MeshStandardMaterial({ color: 0x5a5a5a });

    const base = new THREE.Mesh(baseGeom, baseMat);
    base.position.y = baseHeight / 2;
    group.add(base);

    // ---- Deck superior ----
    const deckGeom = new THREE.BoxGeometry(width, deckThickness, depth);
    const deckMat = new THREE.MeshStandardMaterial({ color: 0x8a8a8a });

    const deck = new THREE.Mesh(deckGeom, deckMat);
    deck.position.y = baseHeight + deckThickness / 2;
    group.add(deck);

    // ---- Bollards ----
    const bollardGeom = new THREE.CylinderGeometry(0.25, 0.25, 0.6, 16);
    const bollardMat = new THREE.MeshStandardMaterial({ color: 0xffd000 });

    const bollardCount = Math.max(2, Math.floor(width / 6));
    for (let i = 0; i < bollardCount; i++) {
      const x = -width / 2 + (i * width) / (bollardCount - 1);
      const bollard = new THREE.Mesh(bollardGeom, bollardMat);
      bollard.position.set(x, baseHeight + 0.45, -depth / 2 + 0.35);
      group.add(bollard);
    }

    // Nivelado com Warehouse
    group.position.copy(position);
    group.position.y = 0;

    group.name = `Dock_${id}`;

    return group;
  }
}
