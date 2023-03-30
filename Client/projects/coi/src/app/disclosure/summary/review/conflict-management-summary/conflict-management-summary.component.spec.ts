import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ConflictManagementSummaryComponent } from './conflict-management-summary.component';

describe('ConflictManagementSummaryComponent', () => {
  let component: ConflictManagementSummaryComponent;
  let fixture: ComponentFixture<ConflictManagementSummaryComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ConflictManagementSummaryComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ConflictManagementSummaryComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
