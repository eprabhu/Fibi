/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { UnsavedChangesWarningComponent } from './unsaved-changes-warning.component';

describe('UnsavedChangesWarningComponent', () => {
  let component: UnsavedChangesWarningComponent;
  let fixture: ComponentFixture<UnsavedChangesWarningComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ UnsavedChangesWarningComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(UnsavedChangesWarningComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
