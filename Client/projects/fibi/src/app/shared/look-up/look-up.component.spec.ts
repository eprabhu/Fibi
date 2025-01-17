
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { LookUpComponent } from './look-up.component';

describe('LookUpComponent', () => {
  let component: LookUpComponent;
  let fixture: ComponentFixture<LookUpComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ LookUpComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(LookUpComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
