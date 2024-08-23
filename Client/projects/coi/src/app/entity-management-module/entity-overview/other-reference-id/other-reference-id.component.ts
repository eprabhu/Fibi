import { Component, Input } from '@angular/core';
import { OtheReferenceId } from '../../shared/entity-interface';
import { hideModal, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';

@Component({
  selector: 'app-other-reference-id',
  templateUrl: './other-reference-id.component.html',
  styleUrls: ['./other-reference-id.component.scss']
})
export class OtherReferenceIdComponent {

    otherReferenceIdObj: OtheReferenceId = new OtheReferenceId();
    coiCurrencyOptions = 'currency#CURRENCY_CODE#true#true';
    @Input() sectionName: any;
    @Input() sectionId: any;
    mandatoryList = new Map();

    addOtherRefId(event) {
        openModal('otherReferenceIdModal');
    }

    onReferenceIdTypeSelected(event) {
        if(event) {
            this.otherReferenceIdObj.referenceType = event.code;
        } else {
            this.otherReferenceIdObj.referenceType = null;
        }
    }

    addOtherReferenceID() {
        this.entityMandatoryValidation();
        if(!this.mandatoryList.size) {
            this.clearOtherReferenceID();
        }
    }

    clearOtherReferenceID() {
        this.mandatoryList.clear();
        this.otherReferenceIdObj = new OtheReferenceId();
        hideModal('otherReferenceIdModal');
    }

    entityMandatoryValidation(): void {
        this.mandatoryList.clear();
        if(!this.otherReferenceIdObj.referenceType) {
            this.mandatoryList.set('referenceType', 'Please select reference type.');
        }
        if(!this.otherReferenceIdObj.referenceId) {
            this.mandatoryList.set('referenceId', 'Please enter reference id.');
        }
        if(!this.otherReferenceIdObj.description) {
            this.mandatoryList.set('description', 'Please enter reference description.');
        }
    }
}
