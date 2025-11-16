import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DockView } from './dock-view';

describe('DockView', () => {
  let component: DockView;
  let fixture: ComponentFixture<DockView>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [DockView]
    })
    .compileComponents();

    fixture = TestBed.createComponent(DockView);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
