import { ComponentFixture, TestBed } from '@angular/core/testing';

import { VesselTypes } from './vessel-types';

describe('VesselTypes', () => {
  let component: VesselTypes;
  let fixture: ComponentFixture<VesselTypes>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [VesselTypes]
    })
    .compileComponents();

    fixture = TestBed.createComponent(VesselTypes);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
