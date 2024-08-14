import { ComponentFixture, TestBed } from '@angular/core/testing';

import { EntityRiskComponent } from './entity-risk.component';

describe('EntityRiskComponent', () => {
  let component: EntityRiskComponent;
  let fixture: ComponentFixture<EntityRiskComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EntityRiskComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(EntityRiskComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
