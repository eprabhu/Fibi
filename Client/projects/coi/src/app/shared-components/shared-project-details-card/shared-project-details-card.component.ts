import { Component, Input } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { getFormattedSponsor } from '../../common/utilities/custom-utilities';

@Component({
    selector: 'app-shared-project-details-card',
    templateUrl: './shared-project-details-card.component.html',
    styleUrls: ['./shared-project-details-card.component.scss']
})
export class SharedProjectDetailsCardComponent {

    @Input() projectDetails: any = null;
    @Input() uniqueId: string | number = '';

    getFormattedSponsor = getFormattedSponsor;

    constructor(public commonService: CommonService) {}

}
