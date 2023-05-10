/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { CountModalComponent } from './count-modal.component';

describe('CountModalComponent', () => {
  let component: CountModalComponent;
  let fixture: ComponentFixture<CountModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ CountModalComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CountModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
