import * as THREE from 'three';

export interface VesselOptions {
  maxRows: number;
  maxTiers: number;
  maxBays: number;
  vesselId?: string;
  position?: THREE.Vector3;
}

export class VesselBuilder {

  private static textureLoader = new THREE.TextureLoader();

  static createVessel(options: VesselOptions): THREE.Group {
    const { maxRows, maxTiers, maxBays, vesselId = '0', position = new THREE.Vector3() } = options;

    const vessel = new THREE.Group();
    vessel.name = `Vessel_${vesselId}`;

    // container dims
    const CONTAINER_W = 2.5;
    const CONTAINER_H = 2.5;
    const CONTAINER_D = 6;
    const gapX = 0.25;
    const gapY = 0.15;
    const gapZ = 0.25;

    const containersWidth = maxRows * (CONTAINER_W + gapX) - gapX;
    const containersLength = maxBays * (CONTAINER_D + gapZ) - gapZ;
    const containersHeight = maxTiers * (CONTAINER_H + gapY) - gapY;

    const hullWidth = Math.max(containersWidth + 12, 18);
    const hullLength = Math.max(containersLength + 24, 40);
    const hullDepth = Math.max(5, containersHeight * 0.25 + 4);

    // hull
    const hull = this.createHull(hullLength, hullWidth, hullDepth);
    hull.name = 'hull';
    hull.rotation.x = Math.PI / 2;
    hull.position.y = -(hullLength / 2 - hullDepth / 2);
    hull.position.z = -(hullDepth / 2 - hullLength / 2);
    vessel.add(hull);

    // deck
    const deck = this.createDeck(hullWidth, hullLength);
    deck.name = 'deck';
    deck.position.y = hullDepth * 0.5 + 0.25;
    vessel.add(deck);

    // bridge
    const bridge = this.createBridge(hullWidth, hullLength, Math.max(2, Math.floor(maxTiers / 2)));
    bridge.name = 'bridge';
    bridge.position.z = hullLength / 2 - Math.max(6, bridge.userData["depth"] || 8) - 1.0;
    bridge.position.y = deck.position.y + (bridge.userData["height"] || 6) / 2 + 0.25;
    vessel.add(bridge);

    // funnels
    const funnel1 = this.createFunnel();
    const funnel2 = this.createFunnel();
    const funnelSpacing = 1.4;
    funnel1.position.set(-funnelSpacing / 2, bridge.position.y + hullLength * 0.145, bridge.position.z);
    funnel2.position.set(funnelSpacing / 2, bridge.position.y + hullLength * 0.145, bridge.position.z);
    vessel.add(funnel1);
    vessel.add(funnel2);

    // container area
    const containerArea = new THREE.Group();
    containerArea.name = 'containerArea';
    const startX = - ( (maxRows - 1) * (CONTAINER_W + gapX) ) / 2;
    const startZ = - ( (maxBays - 1) * (CONTAINER_D + gapZ) ) / 2;
    const baseY = deck.position.y + 0.25 + CONTAINER_H / 2;

    const containerGeom = new THREE.BoxGeometry(CONTAINER_W, CONTAINER_H, CONTAINER_D);


    for (let r = 0; r < maxRows; r++) {
      for (let b = 0; b < maxBays; b++) {
        for (let t = 0; t < maxTiers; t++) {
          type ContainerColor = "blue" | "red" | "green" | "black" | "white" | "yellow";
          const colors: ContainerColor[] = ["blue", "red", "green", "black", "white", "yellow"];
          const color: ContainerColor = colors[Math.floor(Math.random() * colors.length)];
          const materials = VesselBuilder.getContainerMaterials(color);


          const c = new THREE.Mesh(containerGeom, materials);

          c.castShadow = true;
          c.position.x = startX + r * (CONTAINER_W + gapX);
          c.position.z = startZ + b * (CONTAINER_D + gapZ);
          c.position.y = baseY + t * (CONTAINER_H);
          containerArea.add(c);
        }
      }
    }
    containerArea.userData = { width: containersWidth, length: containersLength };
    vessel.add(containerArea);

    // railings
    const rails = this.createRails(hullWidth, hullLength, deck.position.y);
    vessel.add(rails);

    vessel.position.copy(position);
    vessel.rotation.y = Math.PI / 2;
    vessel.position.y = -1;
    vessel.traverse(obj => {
      if (obj instanceof THREE.Mesh) {
        obj.castShadow = true;    // projeta sombra
        obj.receiveShadow = true; // recebe sombra
      }
    });
    return vessel;
  }
  private static getContainerMaterials(color: "blue" | "red" | "green" | "black" | "white" | "yellow"): THREE.Material[] {
    const base = `assets/textures/container/${color}`;

    const texRight  = this.textureLoader.load(`${base}_lateral_side.jpg`);
    const texLeft   = this.textureLoader.load(`${base}_lateral_side.jpg`);
    const texTop    = this.textureLoader.load(`${base}_lateral_side.jpg`);
    const texBottom = this.textureLoader.load(`${base}_lateral_side.jpg`);
    const texFront  = this.textureLoader.load(`${base}_back_side.jpg`);
    const texDoor   = this.textureLoader.load(`${base}_door_side.jpg`);

    const all = [texRight, texLeft, texTop, texBottom, texFront, texDoor];
    all.forEach(t => {
      t.colorSpace = THREE.SRGBColorSpace;
      t.wrapS = t.wrapT = THREE.RepeatWrapping;
    });

    return [
      new THREE.MeshStandardMaterial({ map: texRight }),
      new THREE.MeshStandardMaterial({ map: texLeft }),
      new THREE.MeshStandardMaterial({ map: texTop }),
      new THREE.MeshStandardMaterial({ map: texBottom }),
      new THREE.MeshStandardMaterial({ map: texFront }),
      new THREE.MeshStandardMaterial({ map: texDoor })
    ];
  }


  // ---------------- helper builders ----------------

  private static createHull(length: number, width: number, depth: number): THREE.Mesh {
    const shape = new THREE.Shape();
    const halfW = width / 2;
    const keelDepth = depth;

    shape.moveTo(-halfW, 0);
    shape.quadraticCurveTo(-halfW * 0.6, keelDepth * 0.55, -halfW * 0.15, keelDepth * 0.9);
    shape.quadraticCurveTo(0, keelDepth, halfW * 0.15, keelDepth * 0.9);
    shape.quadraticCurveTo(halfW * 0.6, keelDepth * 0.55, halfW, 0);
    shape.lineTo(-halfW, 0);

    const extrudeSettings: any = { steps: 10, depth: length, bevelEnabled: false };
    const geom = new (THREE as any).ExtrudeGeometry(shape, extrudeSettings);
    geom.rotateX(Math.PI / 2);
    geom.translate(0, keelDepth / 2, -length / 2);

    const texture = this.textureLoader.load('assets/textures/vessel/hull.jpg');
    texture.wrapS = texture.wrapT = THREE.RepeatWrapping;
    texture.repeat.set(2, 1);

    const mat = new THREE.MeshStandardMaterial({
      map: texture,
      roughness: 0.8,
      metalness: 0.08
    });

    const mesh = new THREE.Mesh(geom, mat);
    mesh.castShadow = true;
    mesh.receiveShadow = true;
    return mesh;
  }

  private static createDeck(width: number, length: number): THREE.Mesh {
    const geom = new THREE.BoxGeometry(width * 0.98, 0.5, length * 0.98);
    const texture = this.textureLoader.load('assets/textures/vessel/wood.jpg');
    texture.wrapS = texture.wrapT = THREE.RepeatWrapping;
    texture.repeat.set(4, 8);

    const mat = new THREE.MeshStandardMaterial({ map: texture, roughness: 0.6 });
    const mesh = new THREE.Mesh(geom, mat);
    mesh.castShadow = true;
    mesh.receiveShadow = true;
    return mesh;
  }

  private static createBridge(hullWidth: number, hullLength: number, extraFloors: number): THREE.Mesh {
    const baseW = Math.min(12, hullWidth * 0.35);
    const baseD = Math.min(12, hullLength * 0.14);
    const baseH = 5 + extraFloors * 1.6;

    const geom = new THREE.BoxGeometry(baseW, baseH, baseD);
    const texture = this.textureLoader.load('assets/textures/vessel/metal.jpg');
    const mat = new THREE.MeshStandardMaterial({ map: texture, roughness: 0.5 });
    const bridge = new THREE.Mesh(geom, mat);
    bridge.userData = { height: baseH, depth: baseD };

    const whGeom = new THREE.BoxGeometry(baseW * 0.85, Math.max(2, baseH * 0.28), baseD * 0.85);
    const wh = new THREE.Mesh(whGeom, mat.clone());
    wh.position.set(0, baseH * 0.5 + (Math.max(2, baseH * 0.28)) / 2, 0);
    bridge.add(wh);

    const winMat = new THREE.MeshStandardMaterial({ color: 0x111111 });
    const win = new THREE.Mesh(new THREE.PlaneGeometry(baseW * 0.9, 3), winMat);
    win.position.set(0, baseH * 0.12 + 0.6, baseD * 0.5 + 0.01);
    bridge.add(win);

    return bridge;
  }

  private static createFunnel(): THREE.Mesh {
    const geom = new THREE.CylinderGeometry(0.6, 0.6, 3.6, 12);
    const texture = this.textureLoader.load('assets/textures/vessel/metal_dark.jpg');
    const mat = new THREE.MeshStandardMaterial({ map: texture, metalness: 0.6, roughness: 0.35 });
    return new THREE.Mesh(geom, mat);
  }

  private static createRails(hullWidth: number, hullLength: number, deckY: number): THREE.Group {
    const g = new THREE.Group();
    const texture = this.textureLoader.load('assets/textures/vessel/metal_shiny.jpg');
    const mat = new THREE.MeshStandardMaterial({ map: texture, roughness: 0.6 });

    const railHeight = Math.max(0.4, hullWidth * 0.03);
    const railThickness = Math.max(0.12, hullWidth * 0.01);
    const inset = 0.8;

    const left = new THREE.Mesh(new THREE.BoxGeometry(railThickness, railHeight, hullLength * 0.98), mat);
    left.position.set(-hullWidth / 2 + inset, deckY + railHeight / 2, 0);
    g.add(left);

    const right = left.clone();
    right.position.set(hullWidth / 2 - inset, deckY + railHeight / 2, 0);
    g.add(right);

    const front = new THREE.Mesh(new THREE.BoxGeometry(hullWidth * 0.98, railHeight, railThickness), mat);
    front.position.set(0, deckY + railHeight / 2, -hullLength / 2 + inset);
    g.add(front);

    const back = front.clone();
    back.position.set(0, deckY + railHeight / 2, hullLength / 2 - inset);
    g.add(back);

    return g;
  }
}
