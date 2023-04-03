/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { EntityManagementService } from './entity-management.service';

describe('Service: EntityManagement', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [EntityManagementService]
    });
  });

  it('should ...', inject([EntityManagementService], (service: EntityManagementService) => {
    expect(service).toBeTruthy();
  }));
});
