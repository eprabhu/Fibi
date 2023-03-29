/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { EntityRelationshipService } from './entity-relationship.service';

describe('Service: EntityRelationship', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [EntityRelationshipService]
    });
  });

  it('should ...', inject([EntityRelationshipService], (service: EntityRelationshipService) => {
    expect(service).toBeTruthy();
  }));
});
