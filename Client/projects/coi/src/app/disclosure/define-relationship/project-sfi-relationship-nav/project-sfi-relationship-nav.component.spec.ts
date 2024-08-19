/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { ProjectSfiRelationshipNavComponent } from './project-sfi-relationship-nav.component';

describe('ProjectSfiRelationshipNavComponent', () => {
  let component: ProjectSfiRelationshipNavComponent;
  let fixture: ComponentFixture<ProjectSfiRelationshipNavComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ProjectSfiRelationshipNavComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectSfiRelationshipNavComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
