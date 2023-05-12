/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';
import { Training_detailsComponent } from './Training_details.component';

describe('Training_detailsComponent', () => {
  let component: Training_detailsComponent;
  let fixture: ComponentFixture<Training_detailsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ Training_detailsComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(Training_detailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
