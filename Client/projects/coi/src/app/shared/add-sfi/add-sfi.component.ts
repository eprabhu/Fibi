import { Component, ElementRef, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { ActivityService } from '../../../../../fibi/src/app/agreement/agreement-shared/activity-track/activity.service';
import { slideHorizontalFast } from './../../../../../fibi/src/app/common/utilities/animations';
import { SfiService } from '../../disclosure/sfi/sfi.service';
import { environment } from '../../../environments/environment';
import { CommonService } from '../../common/services/common.service';
import { getEndPointOptionsForCountry, getEndPointOptionsForEntity } from '../../../../../fibi/src/app/common/services/end-point.config';
import { hideModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { compareDates, getDateObjectFromTimeStamp, removeTimeZoneFromDateObject } from '../../../../../fibi/src/app/common/utilities/date-utilities';
import { DATE_PLACEHOLDER, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../../fibi/src/app/app-constants';
import { EntityDetails } from '../../entity-management/entity-details-interface';

declare var $: any;
export interface EndpointOptions {
  contextField: string;
  formatString: string;
  path: string;
  defaultValue: string;
  params: string;
}
@Component({
  selector: 'app-add-sfi',
  templateUrl: './add-sfi.component.html',
  styleUrls: ['./add-sfi.component.scss'],
  providers: [ActivityService],
  animations: [slideHorizontalFast]
})
export class AddSfiComponent implements OnInit {

  @Input() disclosureDetails: { disclosureId: any, disclosureNumber: any } = { disclosureId: null, disclosureNumber: null };

  isSaving = false;
  scrollHeight: number;
  entityDetails: EntityDetails = new EntityDetails();
  additionalDetails: any = {
    sponsorsResearch: false
  };
  deployMap = environment.deployUrl;
  @ViewChild('sfiNavOverlay', { static: true }) sfiNavOverlay: ElementRef;
  isAddAttachment = false;
  isAddAssignee = false;
  dateTime: string;
  datePlaceHolder = DATE_PLACEHOLDER;
  isReadMore: false;
  showRelationshipModal = false;
  clearField: any = false;
  EntitySearchOptions: any = {};
  countrySearchOptions: EndpointOptions;
  clearCountryField: any;
  $subscriptions: Subscription[] = [];
  mandatoryList = new Map();
  emailWarningMsg: any;
  sfiLookUpList: any = {};
  isExpandedAdditionalDetails = true;
  isResultFromSearch = false;
  riskLevelLookup = [
    {
      'description': 'High',
      'riskCategoryCode': '1'
    },
    {
      'description': 'Medium',
      'riskCategoryCode': '2'
    },
    {
      'description': 'Low',
      'riskCategoryCode': '3'
    }
  ];
  isEntityManagement = false;
  @Input() coiEntityManageId: any = null;
  @Input() isEditEntity = false;
  @Output() updatedDataStore = new EventEmitter<boolean>();
  @Output() updateEntityList = new EventEmitter<boolean>();
  heading: string;
  buttonName: string;
  btnTitle: string;
  @Input() changeType = '';
  constructor(public sfiService: SfiService, public _commonService: CommonService, private _router: Router) { }

  ngOnInit(): void {
    this.isEntityManagement = this._router.url.includes('entity-management');
    this.setHeader();
    this.getSFILookup();
    this.showSfiNavBar();
    if (this.coiEntityManageId) {
      this.getEntityDetails();
    }
    this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonService.baseUrl);
    this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
  }

  getSFILookup() {
    this.$subscriptions.push(this.sfiService.addSFILookUp().subscribe((res: any) => {
      this.sfiLookUpList = res;
    }));
  }

  removeEntityId() {
    this._router.navigate([], {
      queryParams: { entityId: null },
      queryParamsHandling: 'merge'
    });
  }

  setEntityTypeObj() {
    this.entityDetails.coiEntity.entityType = this.sfiLookUpList.entityType.find(ele =>
      this.entityDetails.coiEntity.entityTypeCode === ele.entityTypeCode);
  }

  hideSfiNavBar() {
    if (!this.isSaving) {
      this.sfiService.isShowSfiNavBar = false;
      this.showSfiNavBar();
      this.removeEntityId();
      this.mandatoryList.clear();
      this.entityDetails.coiEntity.entityTypeCode = null;
      this.additionalDetails = {
        sponsorsResearch: false
      };
      this.clearSFIFields();
    }
  }

  showSfiNavBar() {
    if (this.sfiService.isShowSfiNavBar) {
      this.scrollHeight = document.documentElement.scrollTop;
      document.body.style.overflow = 'hidden';
      document.documentElement.style.top = - this.scrollHeight + 'px';
    } else {
      document.body.style.overflow = 'auto';
      document.documentElement.scrollTop = this.scrollHeight;
    }
  }

  addEntityToggle(event) {
    hideModal(event);
  }

  hideRelationshipModal(event) {
    this.showRelationshipModal = event;
    this.clearSFIFields();
    this.showSfiNavBar();
  }

  createOrUpdateEntitySFI() {
    this.entityDetails.coiEntity.entityId && !this.isEntityManagement ?
      this.saveAdditionalDetails() : this.saveEntityDetails();
  }

  saveEntityDetails() {
    this.$subscriptions.push(this.sfiService.saveOrUpdateCoiEntity(this.entityDetails).subscribe((data: EntityDetails) => {
      this.entityDetails = data;
      this.entityDetails.coiEntity.entityId && !this.isEntityManagement ?
        this.saveAdditionalDetails() : this.updateDetails();
    }, _err => {
      this.isSaving = false;
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
    }));
  }

  updateDetails() {
    this.isEditEntity ? this.updatedDataStore.emit(true) : this.updateEntityList.emit(true);
    this.sfiService.isShowSfiNavBar = false;
    this.isSaving = false;
    // this commented method need for modify entity
    // if (this.changeType === '2') {
    //   $('#actionConfirmationModal').modal('hide');
    // }
    this._commonService.showToast(HTTP_SUCCESS_STATUS, ` ${this.isEditEntity ? 'Update ' : 'Created '}Entity Successfully completed.`);
  }

  saveAdditionalDetails() {
    this.$subscriptions.push(this.sfiService.createSFI(
      {
        personEntity: {
          entityId: this.entityDetails.coiEntity.entityId,
          entityNumber: this.entityDetails.coiEntity.entityNumber,
          ...this.additionalDetails
        },
        ...this.disclosureDetails
      }).subscribe((data: any) => {
        this.additionalDetails = data.personEntity;
        this.isSaving = false;
        this.showRelationshipModal = true;
        this.additionalDetails.involvementStartDate = getDateObjectFromTimeStamp(this.additionalDetails.involvementStartDate);
        this.additionalDetails.involvementEndDate = getDateObjectFromTimeStamp(this.additionalDetails.involvementEndDate);
      }, _err => {
        this.isSaving = false;
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
      }));
  }

  selectNewEntity(event) {
    this.entityDetails.coiEntity.entityName = event.searchString;
  }

  selectedEvent(event) {
    if (event) {
      this.isResultFromSearch = true;
      this.entityDetails.coiEntity = event;
      if (event.country) {
        this.countrySearchOptions.defaultValue = event.country.countryName;
        this.selectedCountryEvent(event.country);
      }
      this.clearCountryField = new String('false');
    } else {
      this.entityDetails = new EntityDetails();
      this.clearCountryField = new String('true');
      this.clearSFIFields();
    }
  }

  selectedCountryEvent(event) {
    if (event) {
      this.entityDetails.coiEntity.countryCode = event.countryCode;
      this.countrySearchOptions.defaultValue = event.countryName;
    } else {
      this.entityDetails.coiEntity.countryCode = null;
    }
  }

  clearSFIFields() {
    this.entityDetails = new EntityDetails();
    this.additionalDetails = {
      sponsorsResearch: false
    };
    this.clearCountryField = new String('true');
    this.clearField = new String('true');
    this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonService.baseUrl);
    this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
    this.isResultFromSearch = false;
    this.mandatoryList.clear();
  }

  checkMandatoryFilled() {
    this.mandatoryList.clear();
    if (!this.entityDetails.coiEntity.entityName) {
      this.mandatoryList.set('entityName', 'Please choose an entity name.');
    }
    if (!this.entityDetails.coiEntity.entityId && !this.isEntityManagement) {
      this.entityDetailsValidation();
    } else if (this.isEntityManagement) {
      this.entityDetailsValidation();
      if (!this.entityDetails.coiEntity.webURL) {
        this.mandatoryList.set('website', '* Please enter a entity website.');
      }
    }
    if (!this.isEntityManagement) {
      if (!this.additionalDetails.involvementStartDate) {
        this.mandatoryList.set('date', 'Please enter a start date.');
      }
      if (!this.additionalDetails.staffInvolvement) {
        this.mandatoryList.set('staff', 'Please enter Relationship with Entity details.');
      }
      if (!this.additionalDetails.studentInvolvement) {
        this.mandatoryList.set('student', 'Please enter Principle Business Area of Entity details.');
      }
      if (!this.additionalDetails.instituteResourceInvolvement) {
        this.mandatoryList.set('resource', 'Please enter Relationship of Entity to your University responsibilities details.');
      }
      this.endDateValidation();
    }
    return this.mandatoryList.size !== 0 || this.emailWarningMsg ? false : true;
  }

  entityDetailsValidation() {
    if (!this.entityDetails.coiEntity.countryCode) {
      this.mandatoryList.set('country', 'Please choose a country.');
    }
    if (!this.entityDetails.coiEntity.entityTypeCode || this.entityDetails.coiEntity.entityTypeCode === 'null') {
      this.mandatoryList.set('entityType', 'Please choose an entity type.');
    }
    if (!this.entityDetails.coiEntity.emailAddress) {
      this.mandatoryList.set('email', 'Please enter a email address.');
    }
    if (!this.entityDetails.coiEntity.address) {
      this.mandatoryList.set('address', 'Please enter an address.');
    }
    if (!this.entityDetails.coiEntity.phone) {
      this.mandatoryList.set('phone', 'Please enter phone number.');
    }
    this.emailValidation();
    if (!this.entityDetails.coiEntity.zipCode) {
      this.mandatoryList.set('zipCode', 'Please enter a zipCode.');
    }
  }
  emailValidation() {
    this.emailWarningMsg = null;
    if (this.entityDetails.coiEntity.emailAddress) {
      this.entityDetails.coiEntity.emailAddress = this.entityDetails.coiEntity.emailAddress.trim();
      if (this.entityDetails.coiEntity.emailAddress !== undefined && this.entityDetails.coiEntity.emailAddress !== '') {
        const email = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)| (".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
        if (!(email.test(String(this.entityDetails.coiEntity.emailAddress).toLowerCase()))) {
          this.emailWarningMsg = 'Please select a valid email address.';
        } else {
          this.emailWarningMsg = null;
        }
      }
    }
  }

  inputRestriction(event: any) {
    const pattern = /[0-9\+\-\/\ ]/;
    if (!pattern.test(String.fromCharCode(event.charCode))) {
      event.preventDefault();
    }
  }

  phoneNumberValidation(input) {
    this.mandatoryList.clear();
    // tslint:disable-next-line:max-line-length
    const pattern = (/^(?:(?:\(?(?:00|\+)([1-4]\d\d|[1-9]\d?)\)?)?[0-9]?)?((?:\(?\d{1,}\)?[\-\.\ \\\/]?){0,})(?:[\-\.\ \\\/]?(?:#|ext\.?|extension|x)[\-\.\ \\\/]?(\d+))?$/);
    if (!pattern.test(input)) {
      this.checkForInvalidPhoneNumber(input);
    }
  }

  checkForInvalidPhoneNumber(input) {
    if (/^([a-zA-Z]|[0-9a-zA-Z])+$/.test(input)) {
      this.mandatoryList.set('phoneNumber', 'Alphabets cannot be added in Phone number field.');
    } else {
      this.mandatoryList.set('phoneNumber', 'Please add a valid number');
    }
  }

  endDateValidation(): void {
    this.mandatoryList.delete('endDate');
    if (this.additionalDetails.involvementStartDate && this.additionalDetails.involvementEndDate &&
      (compareDates(this.additionalDetails.involvementStartDate, this.additionalDetails.involvementEndDate) === 1)) {
      this.mandatoryList.set('endDate', 'Please provide a valid date.');
    }
  }

  setHeader() {
    if (this.isEntityManagement) {
      if (this.isEditEntity) {
        this.buttonName = 'Update Entity';
        this.btnTitle = 'Click here to update an entity';
        this.heading = `Entity ${this.entityDetails.coiEntity.entityName}`;
      } else {
        this.heading = 'Add New Entity';
        this.buttonName = 'Create Entity';
        this.btnTitle = 'Click here to create an entity';
      }
    } else {
      this.heading = 'Significant Financial Interest';
      this.buttonName = 'Create SFI';
      this.btnTitle = 'Click here to create a sfi';
    }
  }
  getEntityDetails() {
    this.$subscriptions.push(this.sfiService.getEntityDetails(this.coiEntityManageId).subscribe((res: EntityDetails) => {
      this.entityDetails = res;
      this.heading = `Entity ${this.entityDetails.coiEntity.entityName}`;
      this.clearCountryField = new String('false');
      this.countrySearchOptions.defaultValue = this.entityDetails.coiEntity.country.countryName;
      this.selectedCountryEvent(res.coiEntity.country);
    }));
  }

  setEntityRiskCategoryObj() {
    this.entityDetails.coiEntity.entityRiskCategory = this.riskLevelLookup.find(ele =>
      this.entityDetails.coiEntity.riskCategoryCode === ele.riskCategoryCode);
  }

  submitEntity() {
    if (!this.checkMandatoryFilled() && !this.isSaving) {
      return null;
    }
    this.createOrUpdateEntitySFI();
    // this commented method need for modify entity
    // this.changeType === '2' ? $('#actionConfirmationModal').modal('show') : this.createOrUpdateEntitySFI();
  }
  updateChanges(event) {
    if (event) {
      this.createOrUpdateEntitySFI();
    }
  }
}
