/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { ProjectOverviewService } from './project-overview.service';

describe('Service: ProjectOverview', () => {
    beforeEach(() => {
        TestBed.configureTestingModule({
            providers: [ProjectOverviewService]
        });
    });

    it('should ...', inject([ProjectOverviewService], (service: ProjectOverviewService) => {
        expect(service).toBeTruthy();
    }));
});
