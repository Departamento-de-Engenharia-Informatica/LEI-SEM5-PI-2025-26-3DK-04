import * as THREE from 'three';

export enum YardCraneType {
  Gantry = "YardGantry",
}

export class YardGantryCraneBuilder {

  static createCrane(
    yardWidth: number,
    yardDepth: number,
    legHeight: number,
    position: THREE.Vector3,
    id: number,
    rotationY: number = 0
  ): THREE.Group {

    const crane = new THREE.Group();
    crane.name = `YGC_${id}`;

    const loader = new THREE.TextureLoader();

    // ======== Materiais com texturas ========
    const yellowMetal = new THREE.MeshStandardMaterial({
      map: loader.load('/assets/textures/crane/yellow_metal.jpg'),
      roughness: 0.35,
      metalness: 0.7,
    });

    const darkMetal = new THREE.MeshStandardMaterial({
      map: loader.load('/assets/textures/crane/dark_metal.jpg'),
      roughness: 0.55,
      metalness: 0.5,
    });

    const redMetal = new THREE.MeshStandardMaterial({
      map: loader.load('/assets/textures/crane/red_metal.jpg'),
      roughness: 0.45,
      metalness: 0.6,
    });

    const glassMat = new THREE.MeshStandardMaterial({
      map: loader.load('/assets/textures/crane/glass.png'),
      transparent: true,
      opacity: 0.7,
      roughness: 0.4,
    });

    // ======== Dimensões base ========
    const girderLength = yardDepth + 10;
    const girderWidth  = 1.4;
    const girderHeight = 1.4;
    const legInset     = 1.2;
    const wheelRadius  = 0.55;

    // --------------------------
    // 1) GIRDER
    // --------------------------
    const girder = new THREE.Group();
    girder.name = 'girder';

    const flangeGeo = new THREE.BoxGeometry(girderLength, 0.35, girderWidth);
    const webGeo    = new THREE.BoxGeometry(girderLength, girderHeight, 0.22);

    const topFlange = new THREE.Mesh(flangeGeo, yellowMetal);
    const bottomFlange = new THREE.Mesh(flangeGeo, yellowMetal);
    bottomFlange.position.y = -girderHeight;
    const web = new THREE.Mesh(webGeo, yellowMetal);
    web.position.y = -girderHeight / 2;

    girder.add(topFlange, bottomFlange, web);

    const guardRailGeo = new THREE.BoxGeometry(girderLength, 0.15, 0.15);
    const leftGuard = new THREE.Mesh(guardRailGeo, yellowMetal);
    const rightGuard = leftGuard.clone();
    leftGuard.position.set(0, 0.45, girderWidth / 2 + 0.22);
    rightGuard.position.set(0, 0.45, - (girderWidth / 2 + 0.22));
    girder.add(leftGuard, rightGuard);

    const diagGeo = new THREE.BoxGeometry(3.0, 0.12, 0.12);
    for (let x = -Math.floor(girderLength / 2) + 3; x < Math.floor(girderLength / 2) - 3; x += 4.5) {
      const d1 = new THREE.Mesh(diagGeo, yellowMetal);
      d1.position.set(x, -0.65, girderWidth / 2 + 0.02);
      d1.rotation.z = Math.PI / 5;
      const d2 = d1.clone();
      d2.position.z = - (girderWidth / 2 + 0.02);
      d2.rotation.z = -Math.PI / 5;
      girder.add(d1, d2);
    }

    girder.position.y = legHeight;
    crane.add(girder);

    // --------------------------
    // 2) PERNAS LATERAIS
    // --------------------------
    const createLeg = (x: number) => {
      const lg = new THREE.Group();
      lg.name = `leg_${x < 0 ? 'L' : 'R'}`;

      const legMesh = new THREE.Mesh(new THREE.BoxGeometry(1.3, legHeight, 1.3), darkMetal);
      legMesh.position.y = legHeight / 2;
      lg.add(legMesh);

      const brace = new THREE.Mesh(new THREE.BoxGeometry(1.6, 0.18, 0.18), darkMetal);
      brace.position.set(0, legHeight * 0.55, 0.6);
      brace.rotation.z = -Math.PI / 6;
      lg.add(brace);

      const plate = new THREE.Mesh(new THREE.BoxGeometry(1.1, 0.28, 2.0), darkMetal);
      plate.position.set(0, 0.14, 0);
      lg.add(plate);

      lg.position.set(x, 0, 0);
      return lg;
    };

    const leftLegX  = -girderLength / 2 + legInset;
    const rightLegX =  girderLength / 2 - legInset;

    const leftLeg  = createLeg(leftLegX);
    const rightLeg = createLeg(rightLegX);

    crane.add(leftLeg, rightLeg);

    // --------------------------
    // 3) RODAS
    // --------------------------
    const createWheelPair = (offsetX: number) => {
      const g = new THREE.Group();
      const wheelGeo = new THREE.CylinderGeometry(wheelRadius, wheelRadius, 0.45, 18);
      const w1 = new THREE.Mesh(wheelGeo, darkMetal);
      const w2 = w1.clone();
      w1.rotation.z = Math.PI / 2;
      w2.rotation.z = Math.PI / 2;
      w1.position.set(0, wheelRadius, -0.6);
      w2.position.set(0, wheelRadius,  0.6);
      const axle = new THREE.Mesh(new THREE.BoxGeometry(0.18, 0.18, 1.6), darkMetal);
      axle.position.set(0, wheelRadius, 0);
      g.add(w1, w2, axle);
      g.position.set(offsetX, 0, 0);
      return g;
    };

    leftLeg.add(createWheelPair(0));
    rightLeg.add(createWheelPair(0));

    // --------------------------
    // 4) RAILS NO CHÃO
    // --------------------------
    const railGeo = new THREE.BoxGeometry(girderLength + 12, 1.25, 0.6);
    const leftRail  = new THREE.Mesh(railGeo, darkMetal);
    const rightRail = leftRail.clone();
    leftRail.position.set(0, 0.625, -2.2);
    rightRail.position.set(0, 0.625,  2.2);
    crane.add(leftRail, rightRail);

    // --------------------------
    // 5) TROLLEY
    // --------------------------
    const trolley = new THREE.Group();
    const body = new THREE.Mesh(new THREE.BoxGeometry(5, 2, 3.6), redMetal);
    trolley.add(body);

    const topWheelGeo = new THREE.CylinderGeometry(0.22, 0.22, 0.36, 16);
    const wheelPositions = [
      [-1.0, -0.18,  girderWidth/2 - 0.12],
      [ 1.0, -0.18,  girderWidth/2 - 0.12],
      [-1.0, -0.18, - (girderWidth/2 - 0.12)],
      [ 1.0, -0.18, - (girderWidth/2 - 0.12)]
    ];
    wheelPositions.forEach((wp) => {
      const tw = new THREE.Mesh(topWheelGeo, darkMetal);
      tw.rotation.x = Math.PI / 2;
      tw.position.set(wp[0], wp[1], wp[2]);
      trolley.add(tw);
    });

    const sideWheelGeo = new THREE.CylinderGeometry(0.14, 0.14, 0.28, 12);
    const swL = new THREE.Mesh(sideWheelGeo, darkMetal);
    const swR = swL.clone();
    swL.rotation.z = Math.PI / 2; swR.rotation.z = Math.PI / 2;
    swL.position.set(0, -0.45, 1.2);
    swR.position.set(0, -0.45, -1.2);
    trolley.add(swL, swR);

    const cableGeo = new THREE.CylinderGeometry(0.055, 0.055, 2.8, 8);
    const cableLeft = new THREE.Mesh(cableGeo, darkMetal);
    const cableRight = new THREE.Mesh(cableGeo, darkMetal);
    cableLeft.position.set(-0.7, -1.4, 0);
    cableRight.position.set(0.7, -1.4, 0);
    trolley.add(cableLeft, cableRight);

    // Spreader + ganchos
    const spreader = new THREE.Group();
    const spreaderBody = new THREE.Mesh(new THREE.BoxGeometry(3.5, 0.5, 2), yellowMetal);
    spreader.add(spreaderBody);

    const lockGeo = new THREE.BoxGeometry(0.35, 0.35, 0.8);
    const lockPositions = [
      [-1.45, -0.25, -0.95],
      [ 1.45, -0.25, -0.95],
      [-1.45, -0.25,  0.95],
      [ 1.45, -0.25,  0.95],
    ];
    lockPositions.forEach(p => {
      const lk = new THREE.Mesh(lockGeo, darkMetal);
      lk.position.set(p[0], p[1], p[2]);
      spreader.add(lk);
    });

    spreader.position.set(0, -3.0, 0);
    trolley.add(spreader);

    trolley.position.set(0, 0.1, 0);
    girder.add(trolley);

    // --------------------------
    // 6) CABINE
    // --------------------------
    const cabin = new THREE.Group();
    const cabinBody = new THREE.Mesh(new THREE.BoxGeometry(2.0, 1.4, 1.4), darkMetal);
    const cabinGlass = new THREE.Mesh(new THREE.BoxGeometry(1.8, 1.2, 0.08), glassMat);
    cabinGlass.position.set(0, 0, 0.71);
    cabin.add(cabinBody, cabinGlass);
    cabin.position.set(-girderLength / 2 + 3.2, 0.5, girderWidth / 2 + 0.9);
    girder.add(cabin);

    // --------------------------
    // Posicionamento global
    // --------------------------
    crane.position.copy(position);
    crane.rotation.y = rotationY + Math.PI / 2;
    crane.traverse(obj => {
      if (obj instanceof THREE.Mesh) {
        obj.castShadow = true;    // projeta sombra
        obj.receiveShadow = true; // recebe sombra
      }
    });
    return crane;
  }
}
