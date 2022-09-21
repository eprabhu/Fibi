/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { ScheduleListService } from './Schedule-list.service';

describe('Service: ScheduleList', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ScheduleListService]
    });
  });

  it('should ...', inject([ScheduleListService], (service: ScheduleListService) => {
    expect(service).toBeTruthy();
  }));
});
