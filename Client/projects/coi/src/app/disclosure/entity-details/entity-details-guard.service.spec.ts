/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { EntityDetailsGuardService } from './entity-details-guard.service';

describe('Service: EntityDetailsGuard', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [EntityDetailsGuardService]
    });
  });

  it('should ...', inject([EntityDetailsGuardService], (service: EntityDetailsGuardService) => {
    expect(service).toBeTruthy();
  }));
});
