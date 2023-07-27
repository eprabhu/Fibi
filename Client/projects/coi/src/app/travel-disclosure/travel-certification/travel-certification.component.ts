import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';
import { TravelDisclosureService } from '../services/travel-disclosure.service';
import { TravelDataStoreService } from '../services/travel-data-store.service';
import { CoiTravelDisclosure, TravelDisclosure } from '../travel-disclosure-interface';
import { fadeInOutHeight } from '../../../../../fibi/src/app/common/utilities/animations';

@Component({
    selector: 'app-travel-certification',
    templateUrl: './travel-certification.component.html',
    styleUrls: ['./travel-certification.component.scss'],
    animations: [fadeInOutHeight]
})
export class TravelCertificationComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];
    dependencies = ['coiDisclosure'];
    isEditMode = true;
    isSaving = false;
    coiDisclosure: any;
    isReadMore = false;
    travelReqObject = new CoiTravelDisclosure();
    travelResObject: TravelDisclosure = new TravelDisclosure();
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

    constructor(public travelService: TravelDisclosureService, private _dataStore: TravelDataStoreService) {
        document.getElementById('COI_SCROLL').scrollTo(0, 0);
    }

    ngOnInit(): void {
    }

    ngOnDestroy(): void {
        this.travelService.setUnSavedChanges(false, '');
        this.travelService.isTravelCertified = false;
    }

    // private listenDataChangeFromStore() {
    //     this.$subscriptions.push(
    //         this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
    //             this.getDataFromStore();
    //         })
    //     );
    // }

    // private getDataFromStore() {
    //     this.travelReqObject = this._dataStore.getTravelDisclosureRO() ?
    //         this._dataStore.getTravelDisclosureRO() : new CoiTravelDisclosure();

    //     this.travelResObject = this._dataStore.getData() ?
    //         this._dataStore.getData() : new TravelDisclosure();

    //     this.travelService.isTravelCertified = this.travelService.isTravelCertified ? true : false;
    // }

    // private certifyTravelDisclosure(): void {
    //     this.travelService.certifyTravelDisclosure(this.travelReqObject).subscribe((res: any) => {
    //         if (res) {
    //             this.travelService.isTravelCertified = true;
    //             this.travelResObject.certifiedAt = res.certifiedAt;
    //             this.travelResObject.certifiedBy = res.certifiedBy;
    //             this._dataStore.manualDataUpdate(this.travelResObject);
    //         }
    //     }, (err) => {
    //         this.travelService.isTravelCertified = false;
    //     });
    // }

    /**
 * @description
 * for validating whether travel disclosure request object contains all values or not
 */
    private validateTravelDisclosureRO() {
        this.travelReqObject = this._dataStore.getTravelDisclosureRO();
        const excludedVariables: (keyof CoiTravelDisclosure)[] = [
            'description',
            this.travelReqObject.isInternationalTravel ? 'travelState' : 'destinationCountry'
        ];

        const hasEmptyTravelReqObject = Object.keys(this.travelReqObject).some(key => {
            if (!excludedVariables.includes(key as keyof CoiTravelDisclosure)) {
                const travelReqObjectValue = this.travelReqObject[key as keyof CoiTravelDisclosure];
                return travelReqObjectValue === null || travelReqObjectValue === undefined || travelReqObjectValue === '';
            }
            return false;
        });

        if (hasEmptyTravelReqObject) {
            this.travelService.isTravelCertified = false;
        } else {
            this.travelService.isTravelCertified = true;
        }
    }

    certify(): void {
        this.travelService.setUnSavedChanges(true, 'Certification');
        if (!this.travelService.isTravelCertified) {
            this.validateTravelDisclosureRO();
        } else {
            this.travelService.isTravelCertified = false;
        }
    }

}
