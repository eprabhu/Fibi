/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { SfiService } from './sfi.service';

describe('Service: Sfi', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [SfiService]
    });
  });

  it('should ...', inject([SfiService], (service: SfiService) => {
    expect(service).toBeTruthy();
  }));
});
