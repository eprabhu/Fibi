/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { EntityComplianceRiskComponent } from './entity-compliance-risk.component';

describe('EntityComplianceRiskComponent', () => {
  let component: EntityComplianceRiskComponent;
  let fixture: ComponentFixture<EntityComplianceRiskComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ EntityComplianceRiskComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(EntityComplianceRiskComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
