/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { CoiReviewCommentsService } from './coi-review-comments.service';

describe('Service: CoiReviewComments', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [CoiReviewCommentsService]
    });
  });

  it('should ...', inject([CoiReviewCommentsService], (service: CoiReviewCommentsService) => {
    expect(service).toBeTruthy();
  }));
});
