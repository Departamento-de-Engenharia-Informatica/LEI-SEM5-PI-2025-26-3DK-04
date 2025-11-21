import * as THREE from 'three';

export interface VesselOptions {
  maxRows: number;     // containers per row (width)
  maxTiers: number;    // container stack height
  maxBays: number;     // containers along ship length
  vesselId?: string;
  position?: THREE.Vector3; // world position
}

export class VesselBuilder {
  /**
   * Main entry - builds a more realistic container vessel.
   * Keeps performance in mind (no heavy subdivisions).
   */
  static createVessel(options: VesselOptions): THREE.Group {
    const { maxRows, maxTiers, maxBays, vesselId = '0', position = new THREE.Vector3() } = options;

    const vessel = new THREE.Group();
    vessel.name = `Vessel_${vesselId}`;

    // container dims -- consistent with your previous values
    const CONTAINER_W = 2.5;
    const CONTAINER_H = 2.5;
    const CONTAINER_D = 6;
    const gapX = 0.25;
    const gapY = 0.15;
    const gapZ = 0.25;

    // container area dimensions
    const containersWidth = maxRows * (CONTAINER_W + gapX) - gapX;
    const containersLength = maxBays * (CONTAINER_D + gapZ) - gapZ;
    const containersHeight = maxTiers * (CONTAINER_H + gapY) - gapY;

    // hull sizing — leave margin around containers
    const hullWidth = Math.max(containersWidth + 12, 18);
    const hullLength = Math.max(containersLength + 24, 40);
    const hullDepth = Math.max(5, containersHeight * 0.25 + 4);

// --- HULL (extruded profile) -------------------------------------------------
    const hull = this.createHull(hullLength, hullWidth, hullDepth);
    hull.name = 'hull';

// rodar 90º para deitar
    hull.rotation.x = Math.PI / 2;

// alinhar verticalmente (Y)
    hull.position.y = -(hullLength / 2 - hullDepth / 2);

// alinhar horizontalmente (Z) — centra no navio
    hull.position.z = -(hullDepth / 2 - hullLength / 2);

    vessel.add(hull);


    // deck
    const deck = this.createDeck(hullWidth, hullLength);
    deck.name = 'deck';
    deck.position.y = hullDepth * 0.5 + 0.25;
    vessel.add(deck);

    // bridge + superstructure (near stern)
    const bridge = this.createBridge(hullWidth, hullLength, Math.max(2, Math.floor(maxTiers / 2)));
    bridge.name = 'bridge';
    bridge.position.z = hullLength / 2 - Math.max(6, bridge.userData["depth"] || 8) - 1.0;
    bridge.position.y = deck.position.y + (bridge.userData["height"] || 6) / 2 + 0.25;
    vessel.add(bridge);

    // funnel placed on top of bridge (two side-by-side)
    const funnel1 = this.createFunnel();
    const funnel2 = this.createFunnel();

    const funnelSpacing = 1.4; // distância entre os centros dos funnels

    funnel1.position.set(
      -funnelSpacing / 2,
      bridge.position.y + hullLength * 0.145,
      bridge.position.z
    );
    funnel2.position.set(
      funnelSpacing / 2,
      bridge.position.y + hullLength * 0.145,
      bridge.position.z
    );

    vessel.add(funnel1);
    vessel.add(funnel2);




    // container area (grouped so you can toggle visibility / remove)
    const containerArea = new THREE.Group();
    containerArea.name = 'containerArea';

    const startX = - ( (maxRows - 1) * (CONTAINER_W + gapX) ) / 2;
    const startZ = - ( (maxBays - 1) * (CONTAINER_D + gapZ) ) / 2;
    const baseY = deck.position.y + 0.25 + CONTAINER_H / 2;

    const containerGeom = new THREE.BoxGeometry(CONTAINER_W, CONTAINER_H, CONTAINER_D);
    const containerColors = [0xff6b6b, 0x6bffb0, 0x6b9bff, 0xffe26b, 0xff6bf0, 0x6bfff1];

    for (let r = 0; r < maxRows; r++) {
      for (let b = 0; b < maxBays; b++) {
        for (let t = 0; t < maxTiers; t++) {
          const mat = new THREE.MeshStandardMaterial({
            color: containerColors[(r + b + t) % containerColors.length],
            roughness: 0.6,
            metalness: 0.2
          });
          const c = new THREE.Mesh(containerGeom, mat);
          c.castShadow = true;
          c.position.x = startX + r * (CONTAINER_W + gapX);
          c.position.z = startZ + b * (CONTAINER_D + gapZ);
          c.position.y = baseY + t * (CONTAINER_H + gapY);
          containerArea.add(c);
        }
      }
    }

    // ensure container area sits centered on deck and not exceeding deck boundaries
    containerArea.userData = { width: containersWidth, length: containersLength };
    vessel.add(containerArea);

    // railings (simple thin boxes) along deck edge
    const rails = this.createRails(hullWidth, hullLength, deck.position.y);
    vessel.add(rails);

    // name + position adjustments
    vessel.position.copy(position);

    // center group's children so vessel origin is center of geometry bounding box
    //this.centerGroupChildren(vessel);

    // orientation: bow faces -Z in your scene
    vessel.rotation.y = Math.PI / 2;
    vessel.position.y = -1; // ensure sits on ground level
    return vessel;
  }

  // ---------------- helper builders ----------------

  private static createHull(length: number, width: number, depth: number): THREE.Mesh {
    // simple but believable hull using an extruded 2D profile
    const shape = new THREE.Shape();
    const halfW = width / 2;
    const keelDepth = depth; // how tall the hull is

    // create a symmetric profile: from stern lower edge -> bow keel -> stern lower edge
    shape.moveTo(-halfW, 0);
    shape.quadraticCurveTo(-halfW * 0.6, keelDepth * 0.55, -halfW * 0.15, keelDepth * 0.9);
    shape.quadraticCurveTo(0, keelDepth, halfW * 0.15, keelDepth * 0.9);
    shape.quadraticCurveTo(halfW * 0.6, keelDepth * 0.55, halfW, 0);
    shape.lineTo(-halfW, 0);

    const extrudeSettings: any = {
      steps: 10,
      depth: length,
      bevelEnabled: false
    };

    // Extrude profile along Z (length). After extrusion we'll rotate to sit on XZ plane.
    const geom = new (THREE as any).ExtrudeGeometry(shape, extrudeSettings);
    geom.rotateX(Math.PI / 2);
    geom.translate(0, keelDepth / 2, -length / 2);

    const mat = new THREE.MeshStandardMaterial({ color: 0x1f3b5a, roughness: 0.8, metalness: 0.08 });
    const mesh = new THREE.Mesh(geom, mat);
    mesh.castShadow = true;
    mesh.receiveShadow = true;
    mesh.position.y = 0; // will be adjusted by caller

    return mesh;
  }

  private static createBulbousBow(radius: number, length: number): THREE.Mesh {
    // small spheroid scaled to look like a bulbous bow
    const geom = new THREE.SphereGeometry(radius, 12, 8);
    geom.scale(1, 0.9, 1.4);
    const mat = new THREE.MeshStandardMaterial({ color: 0x223355, roughness: 0.8 });
    const mesh = new THREE.Mesh(geom, mat);
    mesh.castShadow = true;
    mesh.receiveShadow = true;
    return mesh;
  }

  private static createDeck(width: number, length: number): THREE.Mesh {
    const deckGeom = new THREE.BoxGeometry(width * 0.98, 0.5, length * 0.98);
    const deckMat = new THREE.MeshStandardMaterial({ color: 0x556677, roughness: 0.6 });
    const deck = new THREE.Mesh(deckGeom, deckMat);
    deck.castShadow = true;
    deck.receiveShadow = true;
    return deck;
  }

  private static createBridge(hullWidth: number, hullLength: number, extraFloors: number): THREE.Mesh {
    const baseW = Math.min(12, hullWidth * 0.35);
    const baseD = Math.min(12, hullLength * 0.14);
    const baseH = 5 + extraFloors * 1.6;

    const geom = new THREE.BoxGeometry(baseW, baseH, baseD);
    const mat = new THREE.MeshStandardMaterial({ color: 0xdedede, roughness: 0.5 });
    const bridge = new THREE.Mesh(geom, mat);
    bridge.userData = { height: baseH, depth: baseD };

    // small tier on top for wheelhouse
    const whGeom = new THREE.BoxGeometry(baseW * 0.85, Math.max(2, baseH * 0.28), baseD * 0.85);
    const wh = new THREE.Mesh(whGeom, mat.clone());
    wh.position.set(0, baseH * 0.5 + (Math.max(2, baseH * 0.28)) / 2, 0);
    bridge.add(wh);

    // add simple window plane in front
    const win = new THREE.Mesh(new THREE.PlaneGeometry(baseW * 0.9, 3), new THREE.MeshStandardMaterial({ color: 0x111111 }));
    win.position.set(0, baseH * 0.12 + 0.6, baseD * 0.5 + 0.01);
    bridge.add(win);

    return bridge;
  }

  private static createFunnel(): THREE.Mesh {
    const g = new THREE.CylinderGeometry(0.6, 0.6, 3.6, 12);
    const m = new THREE.MeshStandardMaterial({ color: 0x333333, metalness: 0.6, roughness: 0.35 });
    const f = new THREE.Mesh(g, m);
    return f;
  }

  private static createPropulsion(hullWidth: number, hullDepth: number): THREE.Group {
    const g = new THREE.Group();

    // small shaft housing
    const shaft = new THREE.Mesh(new THREE.BoxGeometry(1.2, 0.6, 0.6), new THREE.MeshStandardMaterial({ color: 0x222222 }));
    shaft.position.set(0, 0.2, 0);
    g.add(shaft);

    // propeller: simple 3-blade made from thin boxes
    const prop = new THREE.Group();
    const bladeGeom = new THREE.BoxGeometry(0.1, 0.02, 1.0);
    const bladeMat = new THREE.MeshStandardMaterial({ color: 0x111111, metalness: 0.8 });
    for (let i = 0; i < 3; i++) {
      const b = new THREE.Mesh(bladeGeom, bladeMat);
      b.position.set(0, 0, 0.5);
      b.rotation.y = (i / 3) * Math.PI * 2;
      prop.add(b);
    }
    prop.rotation.x = Math.PI / 2;
    prop.position.set(0, 0, 0.9);
    g.add(prop);

    // rudder behind prop (small plate)
    const rud = new THREE.Mesh(new THREE.BoxGeometry(0.15, 1.2, 0.02), new THREE.MeshStandardMaterial({ color: 0x2a3a4a }));
    rud.position.set(0, -0.2, 1.6);
    g.add(rud);

    return g;
  }

  private static createRails(hullWidth: number, hullLength: number, deckY: number): THREE.Group {
    const g = new THREE.Group();
    const railMat = new THREE.MeshStandardMaterial({ color: 0x555555, roughness: 0.6 });
    const railHeight = Math.max(0.4, hullWidth * 0.03);
    const railThickness = Math.max(0.12, hullWidth * 0.01);
    const inset = 0.8;

    const left = new THREE.Mesh(new THREE.BoxGeometry(railThickness, railHeight, hullLength * 0.98), railMat);
    left.position.set(-hullWidth / 2 + inset, deckY + railHeight / 2, 0);
    g.add(left);

    const right = left.clone();
    right.position.set(hullWidth / 2 - inset, deckY + railHeight / 2, 0);
    g.add(right);

    const front = new THREE.Mesh(new THREE.BoxGeometry(hullWidth * 0.98, railHeight, railThickness), railMat);
    front.position.set(0, deckY + railHeight / 2, -hullLength / 2 + inset);
    g.add(front);

    const back = front.clone();
    back.position.set(0, deckY + railHeight / 2, hullLength / 2 - inset);
    g.add(back);

    return g;
  }

  private static centerGroupChildren(group: THREE.Group) {
    const box = new THREE.Box3().setFromObject(group);
    const center = new THREE.Vector3();
    box.getCenter(center);
    group.children.forEach((child) => child.position.sub(center));
  }
}
