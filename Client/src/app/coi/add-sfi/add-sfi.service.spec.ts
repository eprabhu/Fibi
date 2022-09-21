/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { AddSfiService } from './add-sfi.service';

describe('Service: AddSfi', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [AddSfiService]
    });
  });

  it('should ...', inject([AddSfiService], (service: AddSfiService) => {
    expect(service).toBeTruthy();
  }));
});
