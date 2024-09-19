import { Component, Input } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { POST_CREATE_DISCLOSURE_ROUTE_URL } from '../../app-constants';
import { heightAnimation } from '../../common/utilities/animations';
import { openInNewTab } from '../../common/utilities/custom-utilities';
import { ProjectKeyPerson } from '../project-hierarchy-slider/services/project-hierarchy-slider.interface';

@Component({
    selector: 'app-disclosure-project-keyperson',
    templateUrl: './disclosure-project-keyperson.component.html',
    styleUrls: ['./disclosure-project-keyperson.component.scss'],
    animations: [heightAnimation('0', '*', 300, 'heightAnimation')]
})
export class DisclosureProjectKeypersonComponent {

    @Input() isShowAllDisclosures = false;
    @Input() uniqueId: string | number = '';
    @Input() keyPersonData = new ProjectKeyPerson();
    
    constructor(public commonService: CommonService) {}

    redirectToFCOIDisclosure(disclosureId: number | null): void {
        if (disclosureId) {
            openInNewTab(POST_CREATE_DISCLOSURE_ROUTE_URL + '?', ['disclosureId'], [disclosureId]);
        }
    }
}
