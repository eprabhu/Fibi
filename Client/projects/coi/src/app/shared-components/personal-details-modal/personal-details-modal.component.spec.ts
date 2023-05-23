/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { Personal_details_modalComponent } from './personal_details_modal.component';

describe('Personal_details_modalComponent', () => {
  let component: Personal_details_modalComponent;
  let fixture: ComponentFixture<Personal_details_modalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ Personal_details_modalComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(Personal_details_modalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
