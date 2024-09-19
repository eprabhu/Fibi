/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { DisclosureProjectKeypersonComponent } from './disclosure-project-keyperson.component';

describe('DisclosureProjectKeypersonComponent', () => {
  let component: DisclosureProjectKeypersonComponent;
  let fixture: ComponentFixture<DisclosureProjectKeypersonComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ DisclosureProjectKeypersonComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DisclosureProjectKeypersonComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
