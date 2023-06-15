/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { TravelToolKitComponent } from './travel-tool-kit.component';

describe('TravelToolKitComponent', () => {
  let component: TravelToolKitComponent;
  let fixture: ComponentFixture<TravelToolKitComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ TravelToolKitComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TravelToolKitComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
