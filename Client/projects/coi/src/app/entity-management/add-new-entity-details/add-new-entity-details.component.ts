import { Component, ElementRef, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { EntityManagementService } from '../entity-management.service';
import { getEndPointOptionsForCountry } from '../../../../../fibi/src/app/common/services/end-point.config';
import { slideHorizontal } from '../../../../../fibi/src/app/common/utilities/animations';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { EntityDetails } from '../entity-details-interface';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from 'projects/fibi/src/app/app-constants';

export interface EndpointOptions {
  contextField: string;
  formatString: string;
  path: string;
  defaultValue: string;
  params: string;
}
@Component({
  selector: 'app-add-new-entity-details',
  templateUrl: './add-new-entity-details.component.html',
  styleUrls: ['./add-new-entity-details.component.scss'],
  animations: [slideHorizontal]

})
export class AddNewEntityDetailsComponent implements OnInit, OnDestroy {

  $subscriptions: Subscription[] = [];
  isSaving = false;
  @ViewChild('entityNavOverlay', { static: true }) entityNavOverlay: ElementRef;
  scrollHeight: number;
  mandatoryList = new Map();
  clearCountryField: any;
  countrySearchOptions: EndpointOptions;
  entityDetails: EntityDetails = new EntityDetails();
  entityTypeList = [];
  @Input() coiEntityManageId: any = null;
  @Input() isEditEntity = false;
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
  ]
  @Output() updatedDataStore = new EventEmitter<boolean>();

  constructor(public entityManagementService: EntityManagementService, private _commonService: CommonService) { }

  ngOnInit() {
    this.showSfiNavBar();
    this.getEntityTypeLookUp();
    if (this.coiEntityManageId) {
      this.getEntityDetails();
    }
    this.countrySearchOptions = getEndPointOptionsForCountry();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  createEntity() {
    const isValid = this.entityValidation();
    if (isValid) {
      this.$subscriptions.push(this.entityManagementService.
        saveOrUpdateCOIEntity(this.entityDetails).subscribe((res: EntityDetails) => {
          this.entityDetails = res;
          if (this.isEditEntity) {
            this.updatedDataStore.emit(true);
          }
          this.entityManagementService.isShowEntityNavBar = false;

          // this._commonService.showToast(HTTP_SUCCESS_STATUS, ` ${this.isEditEntity?'Update ':'Created '}Entity Successfully completed.`);
        }, _err => {
          // this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }
  }

  hideEntityNavBar() {
    if (!this.isSaving) {
      this.entityManagementService.isShowEntityNavBar = false;
      this.showSfiNavBar();
    }
  }

  showSfiNavBar() {
    if (this.entityManagementService.isShowEntityNavBar) {
      this.entityNavOverlay.nativeElement.style.display = 'block';
      this.scrollHeight = document.documentElement.scrollTop;
      document.body.style.overflow = 'hidden';
      document.documentElement.style.top = - this.scrollHeight + 'px';
    } else {
      this.entityNavOverlay.nativeElement.style.display = 'none';
      document.body.style.overflow = 'auto';
      document.documentElement.scrollTop = this.scrollHeight;
    }
  }

  selectedCountryEvent(event) {
    if (event) {
      this.entityDetails.coiEntity.country = event;
      this.entityDetails.coiEntity.countryCode = event.countryCode;
    }
  }

  getEntityTypeLookUp() {
    this.$subscriptions.push(this.entityManagementService.getEntityLookUp().subscribe((res: any) => {
      this.entityTypeList = res.entityType;
    }));
  }

  setEntityTypeObj() {
    this.entityDetails.coiEntity.entityType = this.entityTypeList.find(ele =>
      this.entityDetails.coiEntity.entityTypeCode === ele.entityTypeCode);
  }
  setEntityRiskCategoryObj() {
    this.entityDetails.coiEntity.entityRiskCategory = this.riskLevelLookup.find(ele =>
      this.entityDetails.coiEntity.riskCategoryCode === ele.riskCategoryCode);
  }

  getEntityDetails() {
    this.$subscriptions.push(this.entityManagementService.getEntityDetails(this.coiEntityManageId).subscribe((res: EntityDetails) => {
      this.entityDetails = res;
      this.clearCountryField = new String('false');
      this.countrySearchOptions.defaultValue = this.entityDetails.coiEntity.country.countryName;
      this.selectedCountryEvent(res.coiEntity.country)
      // this.entityDetails.coiEntity.country = res.coiEntity.country;

    }));
  }

  entityValidation() {
    this.mandatoryList.clear();
    const emailPattern = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)| (".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
    if (!this.entityDetails.coiEntity.country) {
      this.mandatoryList.set('country', '* Please choose a country.');
    }
    if (!this.entityDetails.coiEntity.entityName) {
      this.mandatoryList.set('entityName', '* Please enter a entity name.');
    }
    if (!this.entityDetails.coiEntity.entityTypeCode) {
      this.mandatoryList.set('entityType', '* Please choose a entity type.');
    }
    if (!this.entityDetails.coiEntity.address) {
      this.mandatoryList.set('address', '* Please enter a entity address.')
    }
    if (!this.entityDetails.coiEntity.phone) {
      this.mandatoryList.set('phoneNumber', '* Please enter a entity phone number.')
    }
    if (!this.entityDetails.coiEntity.emailAddress) {
      this.mandatoryList.set('email', '* Please enter a entity email address.')
    } else if (!(emailPattern.test(String(this.entityDetails.coiEntity.emailAddress).toLowerCase()))) {
      this.mandatoryList.set('email', '* Please select a valid email address.')
    }
    if (!this.entityDetails.coiEntity.webURL) {
      this.mandatoryList.set('website', '* Please enter a entity website.')
    }
    // if(!this.entityDetails.coiEntity.riskCategoryCode) {
    //   this.mandatoryList.set('riskLevel', '* Please choose a risk level.');
    // }

    return this.mandatoryList.size === 0 ? true : false;
  }

  phoneNumberValidation(event: any) {
    // const phonePattern = (/^(?:(?:\(?(?:00|\+)([1-4]\d\d|[1-9]\d?)\)?)?[0-9]?)?((?:\(?\d{1,}\)?[\-\.\ \\\/]?){0,})(?:[\-\.\ \\\/]?(?:#|ext\.?|extension|x)[\-\.\ \\\/]?(\d+))?$/);
    // if(!phonePattern.test(input)) {
    //   console.log('Ho');

    // }
    const pattern = /[0-9]\d*/;
    if (!pattern.test(String.fromCharCode(event.charCode))) {
      event.preventDefault();
    }
  }
}
