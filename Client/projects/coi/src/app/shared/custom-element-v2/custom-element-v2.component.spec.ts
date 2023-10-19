/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { CustomElementV2Component } from './custom-element-v2.component';

describe('CustomElementV2Component', () => {
  let component: CustomElementV2Component;
  let fixture: ComponentFixture<CustomElementV2Component>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ CustomElementV2Component ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CustomElementV2Component);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
