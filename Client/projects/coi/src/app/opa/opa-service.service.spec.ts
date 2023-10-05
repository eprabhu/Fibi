/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { OpaServiceService } from './opa-service.service';

describe('Service: OpaService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [OpaServiceService]
    });
  });

  it('should ...', inject([OpaServiceService], (service: OpaServiceService) => {
    expect(service).toBeTruthy();
  }));
});
