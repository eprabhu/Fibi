/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { ReviewerDashboardService } from './reviewer-dashboard.service';

describe('Service: ReviewerDashboard', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ReviewerDashboardService]
    });
  });

  it('should ...', inject([ReviewerDashboardService], (service: ReviewerDashboardService) => {
    expect(service).toBeTruthy();
  }));
});
