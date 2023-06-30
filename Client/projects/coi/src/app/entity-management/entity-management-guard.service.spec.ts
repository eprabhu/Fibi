/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { EntityManagementGuardService } from './entity-management-guard.service';

describe('Service: EntityManagementGuard', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [EntityManagementGuardService]
    });
  });

  it('should ...', inject([EntityManagementGuardService], (service: EntityManagementGuardService) => {
    expect(service).toBeTruthy();
  }));
});
