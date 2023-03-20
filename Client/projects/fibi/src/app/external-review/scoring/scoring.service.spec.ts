/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { ScoringService } from './scoring.service';

describe('Service: Scoring', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ScoringService]
    });
  });

  it('should ...', inject([ScoringService], (service: ScoringService) => {
    expect(service).toBeTruthy();
  }));
});
