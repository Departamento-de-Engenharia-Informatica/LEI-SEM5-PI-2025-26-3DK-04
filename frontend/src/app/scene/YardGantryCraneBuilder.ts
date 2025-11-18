import * as THREE from 'three';

export enum YardCraneType {
  Gantry = "YardGantry",
}

export class YardGantryCraneBuilder {

  /**
   * Cria uma Yard Gantry Crane (RMG-like) realista, sem peças soltas.
   * - yardWidth: largura da yard (X) — usado só para referência/escala da cabine se necessário
   * - yardDepth: profundidade da yard (Z) — comprimento da bridge (girder) vai seguir este eixo
   * - legHeight: altura das pernas (Y)
   * - position: posição central onde colocar a grua
   */
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

    // ======== Materiais ========
    const yellowMetal = new THREE.MeshStandardMaterial({ color: 0xffb200, roughness: 0.35, metalness: 0.7 });
    const darkMetal   = new THREE.MeshStandardMaterial({ color: 0x2c2c2c, roughness: 0.55, metalness: 0.5 });
    const redMetal    = new THREE.MeshStandardMaterial({ color: 0xd30000, roughness: 0.45, metalness: 0.6 });
    const glassMat    = new THREE.MeshStandardMaterial({ color: 0x9fc7ff, transparent: true, opacity: 0.7, roughness: 0.4 });

    // ======== Dimensões base ========
    const girderLength = yardDepth + 10;   // bridge corre na profundidade (Z)
    const girderWidth  = 1.4;
    const girderHeight = 1.4;
    const legInset     = 1.2;               // distancia interior das pernas relativamente às pontas da bridge
    const wheelRadius  = 0.55;

    // --------------------------
    // 1) GIRDER (grupo) — topology: girder -> (topBeam, bottomBeam, web, guards, trolley, cabin)
    // --------------------------
    const girder = new THREE.Group();
    girder.name = 'girder';

    // flange superior e inferior (I-beam style) e web
    const flangeGeo = new THREE.BoxGeometry(girderLength, 0.35, girderWidth);
    const webGeo    = new THREE.BoxGeometry(girderLength, girderHeight, 0.22);

    const topFlange = new THREE.Mesh(flangeGeo, yellowMetal);
    const bottomFlange = new THREE.Mesh(flangeGeo, yellowMetal);
    bottomFlange.position.y = -girderHeight;

    const web = new THREE.Mesh(webGeo, yellowMetal);
    web.position.y = -girderHeight / 2;

    topFlange.name = 'topFlange';
    girder.add(topFlange, bottomFlange, web);

    // guard rails laterais (fixos na bridge)
    const guardRailGeo = new THREE.BoxGeometry(girderLength, 0.15, 0.15);
    const leftGuard = new THREE.Mesh(guardRailGeo, yellowMetal);
    const rightGuard = leftGuard.clone();
    leftGuard.position.set(0, 0.45, girderWidth / 2 + 0.22);
    rightGuard.position.set(0, 0.45, - (girderWidth / 2 + 0.22));
    girder.add(leftGuard, rightGuard);

    // diagonais decorativas / reforço (curtas) - pares, espelhadas
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

    // posição vertical do girder (topo das pernas)
    girder.position.y = legHeight;
    crane.add(girder);

    // --------------------------
    // 2) PERNAS LATERAIS (crane root -> legs)
    // --------------------------
    // cada perna é um grupo para podermos anexar rodas e suportes
    const createLeg = (x: number) => {
      const lg = new THREE.Group();
      lg.name = `leg_${x < 0 ? 'L' : 'R'}`;

      // perna principal
      const legMesh = new THREE.Mesh(new THREE.BoxGeometry(1.3, legHeight, 1.3), darkMetal);
      legMesh.position.y = legHeight / 2;
      lg.add(legMesh);

      // reforço diagonal na perna (visual)
      const brace = new THREE.Mesh(new THREE.BoxGeometry(1.6, 0.18, 0.18), darkMetal);
      brace.position.set(0, legHeight * 0.55, 0.6);
      brace.rotation.z = -Math.PI / 6;
      lg.add(brace);

      // base plate (ligação perna-rodas)
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
    // 3) RODA SETS (na base das pernas) — agrupadas e filhas da perna (sem flutuar)
    // --------------------------
    const createWheelPair = (offsetX: number) => {
      const g = new THREE.Group();
      g.name = 'wheelPair';

      const wheelGeo = new THREE.CylinderGeometry(wheelRadius, wheelRadius, 0.45, 18);
      const w1 = new THREE.Mesh(wheelGeo, darkMetal);
      const w2 = w1.clone();
      w1.rotation.z = Math.PI / 2;
      w2.rotation.z = Math.PI / 2;
      w1.position.set(0, wheelRadius, -0.6);
      w2.position.set(0, wheelRadius,  0.6);

      // eixo
      const axle = new THREE.Mesh(new THREE.BoxGeometry(0.18, 0.18, 1.6), darkMetal);
      axle.position.set(0, wheelRadius, 0);

      g.add(w1, w2, axle);
      g.position.set(offsetX, 0, 0);
      return g;
    };

    // anexa as rodas como filhas das respetivas pernas (garante não flutuar)
    leftLeg.add(createWheelPair(0));
    rightLeg.add(createWheelPair(0));

    // --------------------------
    // 4) RAILS NO CHÃO (visual) — filhas da crane root (posicionadas no chão)
    // --------------------------
    const railGeo = new THREE.BoxGeometry(girderLength + 12, 0.25, 0.6);
    const leftRail  = new THREE.Mesh(railGeo, darkMetal);
    const rightRail = leftRail.clone();
    leftRail.position.set(0, 0.12, -2.2);
    rightRail.position.set(0, 0.12,  2.2);
    crane.add(leftRail, rightRail);

    // --------------------------
  // TROLLEY CORRIGIDO (filho do girder)
  // --------------------------
    const trolley = new THREE.Group();
    trolley.name = 'trolley';

    // corpo principal do trolley
    const body = new THREE.Mesh(new THREE.BoxGeometry(5, 2, 3.6), redMetal);
    body.position.set(0, 0, 0);
    trolley.add(body);

    // rodas superiores do trolley (encaixadas no topo do girder)
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

    // rodas laterais guia
    const sideWheelGeo = new THREE.CylinderGeometry(0.14, 0.14, 0.28, 12);
    const swL = new THREE.Mesh(sideWheelGeo, darkMetal);
    const swR = swL.clone();
    swL.rotation.z = Math.PI / 2; swR.rotation.z = Math.PI / 2;
    swL.position.set(0, -0.45, 1.2);
    swR.position.set(0, -0.45, -1.2);
    trolley.add(swL, swR);

    // cabos do spreader
    // cabos do spreader (verticais agora)
    const cableGeo = new THREE.CylinderGeometry(0.055, 0.055, 2.8, 8);

    const cableLeft = new THREE.Mesh(cableGeo, darkMetal);
    const cableRight = new THREE.Mesh(cableGeo, darkMetal);

    // NÃO rotacionar em X! (mantém vertical)
    cableLeft.position.set(-0.7, -1.4, 0);   // -1.4 é meio comprimento do cabo
    cableRight.position.set(0.7, -1.4, 0);

    trolley.add(cableLeft, cableRight);


    // spreader + ganchos
    const spreader = new THREE.Group();
    spreader.name = 'spreader';
    const spreaderBody = new THREE.Mesh(new THREE.BoxGeometry(3.5, 0.5, 2), yellowMetal);
    spreaderBody.position.set(0, 0, 0);
    spreader.add(spreaderBody);

// ganchos nos 4 cantos (maiores e mais “ganchudos”)
    const lockGeo = new THREE.BoxGeometry(0.35, 0.35, 0.8);  // aumentado
    const lockPositions = [
      [-1.45, -0.25, -0.95],  // canto frente-esquerdo
      [ 1.45, -0.25, -0.95],  // canto frente-direito
      [-1.45, -0.25,  0.95],  // canto trás-esquerdo
      [ 1.45, -0.25,  0.95],  // canto trás-direito
    ];
    lockPositions.forEach(p => {
      const lk = new THREE.Mesh(lockGeo, darkMetal);
      lk.position.set(p[0], p[1], p[2]);
      spreader.add(lk);
    });

    spreader.position.set(0, -3.0, 0);
    trolley.add(spreader);


// posiciona trolley **relativo ao topo do girder**
    trolley.position.set(0, 0.1, 0); // topo do girder + ligeiro encaixe
    girder.add(trolley);

// --------------------------
// CABINE DO OPERADOR CORRIGIDA (filha do girder)
// --------------------------
    const cabin = new THREE.Group();
    cabin.name = 'operatorCabin';
    const cabinBody = new THREE.Mesh(new THREE.BoxGeometry(2.0, 1.4, 1.4), darkMetal);
    const cabinGlass = new THREE.Mesh(new THREE.BoxGeometry(1.8, 1.2, 0.08), glassMat);
    cabinGlass.position.set(0, 0, 0.71);
    cabin.add(cabinBody, cabinGlass);

  // posiciona a cabine **relativa ao girder**
    cabin.position.set(-girderLength / 2 + 3.2, 0.5, girderWidth / 2 + 0.9);
    girder.add(cabin);


    // --------------------------
    // Final: posição global e rotação
    // --------------------------
    crane.position.copy(position);
    crane.rotation.y = rotationY + Math.PI / 2;

    return crane;
  }
}
