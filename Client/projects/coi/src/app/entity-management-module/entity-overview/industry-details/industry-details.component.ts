import { Component } from '@angular/core';
import { hideModal, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Industry_Details } from '../../shared/entity-interface';

@Component({
  selector: 'app-industry-details',
  templateUrl: './industry-details.component.html',
  styleUrls: ['./industry-details.component.scss']
})
export class IndustryDetailsComponent {

    industryDetails: Industry_Details = new Industry_Details();
    industryCategoryTypeOptions = 'INDUSTRY_CATEGORY_TYPE#INDUSTRY_CATEGORY_TYPE#true#true';
    industryCategoryDescriptionOptions = 'currency#CURRENCY_CODE#true#true';
    mandatoryList = new Map();

    addIndustryDetails(event) {
        if(event) {
            openModal('addIndustryDetails');
        }
    }

    clearIndustryDetails() {
        this.mandatoryList.clear();
        this.industryDetails = new Industry_Details();
        hideModal('addIndustryDetails');
    }

    addIndustry() {
        this.entityMandatoryValidation();
        if(!this.mandatoryList.size) {
            this.clearIndustryDetails();
        }
    }

    onIndustryCategoryTypeSelect(event) {
        if(event) {
            this.industryDetails.industryCategoryType = event;
        } else {
            this.industryDetails.industryCategoryType = null;
        }
    }

    entityMandatoryValidation(): void {
        this.mandatoryList.clear();
        if(!this.industryDetails.industryCategroyDescription || !this.industryDetails.industryCategroyDescription.length) {
            this.mandatoryList.set('industryCategroyDescription', 'Please select industry category description.');
        }
        if(!this.industryDetails.industryCategoryType) {
            this.mandatoryList.set('industryCategoryType', 'Please select industry category type.');
        }
    }

    onIndustryCategoryDescriptionSelect(event) {
        if(event) {
            this.industryDetails.industryCategroyDescription = event;
        } else {
            this.industryDetails.industryCategroyDescription = null;
        }
    }

}
