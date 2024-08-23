import { ComponentFixture, TestBed } from '@angular/core/testing';

import { EntityCommonCardComponent } from './entity-common-card.component';

describe('EntityCommonCardComponent', () => {
  let component: EntityCommonCardComponent;
  let fixture: ComponentFixture<EntityCommonCardComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EntityCommonCardComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(EntityCommonCardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
