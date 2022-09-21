/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { CoiDashboardService } from './coi-dashboard.service';

describe('Service: CoiDashboard', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [CoiDashboardService]
    });
  });

  it('should ...', inject([CoiDashboardService], (service: CoiDashboardService) => {
    expect(service).toBeTruthy();
  }));
});
