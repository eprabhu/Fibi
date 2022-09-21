/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { CommiteeListService } from './commitee-list.service';

describe('Service: CommiteeList', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [CommiteeListService]
    });
  });

  it('should ...', inject([CommiteeListService], (service: CommiteeListService) => {
    expect(service).toBeTruthy();
  }));
});
