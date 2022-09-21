/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { ResolveServiceService } from './resolve-service.service';

describe('Service: ResolveService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ResolveServiceService]
    });
  });

  it('should ...', inject([ResolveServiceService], (service: ResolveServiceService) => {
    expect(service).toBeTruthy();
  }));
});
