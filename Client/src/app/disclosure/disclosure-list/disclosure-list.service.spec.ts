/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { DisclosureListService } from './disclosure-list.service';

describe('Service: DisclosureList', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [DisclosureListService]
    });
  });

  it('should ...', inject([DisclosureListService], (service: DisclosureListService) => {
    expect(service).toBeTruthy();
  }));
});
