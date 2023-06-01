import { Component, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';
import { TravelDisclosureService } from '../travel-disclosure.service';

@Component({
    selector: 'app-travel-certification',
    templateUrl: './travel-certification.component.html',
    styleUrls: ['./travel-certification.component.scss']
})
export class TravelCertificationComponent implements OnDestroy {

    $subscriptions: Subscription[] = [];
    dependencies = ['coiDisclosure'];
    isEditMode = true;
    isSaving = false;
    coiDisclosure: any;
    isReadMore = false;
    certificationText = `I agree to abide by the University COI policy guidelines and certify that the information
    provided for the Financial conflict of interest, including, responses to screening questions, list of my pertinent
    Significant Financial interests and possible relationship to my sponsored activity is an accurate and current
    statement of my reportable outside interests and activities.`;
    headerInfoText = `University policy requires that university officers, faculty, and staff and others acting on its
    behalf avoid ethical, legal, financial, and other conflicts of interest and ensure that their activities and
    interests do not conflict with their obligations to the University. Disclosure of financial interests enables
    the University to determine if a financial interest creates a conflict of interest or the appearance of a
    conflict of interest. The existence of a conflict or the appearance of one does not imply wrongdoing and
    does not necessarily mean that a researcher may not retain his or her financial interest and undertake the
    affected research. Often the University can work with the researcher to manage a conflict or the appearance
    of a conflict so that the research can continue in a way that minimizes the possibility of bias and preserves
    the objectivity of the research. Proper management depends on full and prompt disclosure. COI provides the ability
    to disclose and maintain your Significant Financial Interests; identify potential areas of concern related to your
    proposals and awards; and, disclose reimbursed travel (for NIH compliance).`;

    constructor(public travelService: TravelDisclosureService) { }

    ngOnDestroy(): void {
        this.travelService.isTravelCertified = false;
    }

    certifyTravelDisclosure(): void {
        this.travelService.travelDataChanged = true;
        this.travelService.isTravelCertified = true;
    }
}
