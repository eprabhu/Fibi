/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { DisclosureHistoryCardComponent } from './disclosure-history-card.component';

describe('DisclosureHistoryCardComponent', () => {
  let component: DisclosureHistoryCardComponent;
  let fixture: ComponentFixture<DisclosureHistoryCardComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ DisclosureHistoryCardComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DisclosureHistoryCardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
