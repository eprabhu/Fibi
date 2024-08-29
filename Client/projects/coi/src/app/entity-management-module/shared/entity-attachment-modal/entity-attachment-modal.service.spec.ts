/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { EntityAttachmentModalService } from './entity-attachment-modal.service';

describe('Service: EntityAttachmentModal', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [EntityAttachmentModalService]
    });
  });

  it('should ...', inject([EntityAttachmentModalService], (service: EntityAttachmentModalService) => {
    expect(service).toBeTruthy();
  }));
});
