
import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { EndpointOptions } from '../../admin-modules/person-training/person-training.interface';
import { CommonService } from '../../common/services/common.service';
import { getEndPointOptionsForCountry, getEndPointOptionsForEntity } from '../../common/services/end-point.config';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../common/utilities/date-utilities';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { CoiEntity, CoiFinancialEntity } from './add-sfi.interface';
import { AddSfiService } from './add-sfi.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { NavigationService } from '../../common/services/navigation.service';
@Component({
  selector: 'app-add-sfi',
  templateUrl: './add-sfi.component.html',
  styleUrls: ['./add-sfi.component.css']
})
export class AddSfiComponent implements OnInit, OnDestroy {

  clearField: any = false;
  countrySearchOptions: EndpointOptions;
  $subscriptions: Subscription[] = [];
  sfiLookUpList: any = {};
  entityDetails: CoiFinancialEntity = new CoiFinancialEntity();
  searchEntityList: any = {};
  EntitySearchOptions: EndpointOptions;
  coiFinancialEntity: any;
  isSaved = false;
  mandatoryList = new Map();
  clearCountryField: any;
  isShowCollapse = true;
  isSaving = false;
  emailWarningMsg: any;
  isShowDescription = true;
  isViewMode = true;

  constructor(
    private _router: Router,
    public _addSFIService: AddSfiService,
    public _commonService: CommonService,
    private _activatedRoute: ActivatedRoute,
    private _navigationService: NavigationService
  ) { }

  ngOnInit() {
    this.sfiLookUpList = this._addSFIService.lookups;
    this.countrySearchOptions = getEndPointOptionsForCountry();
    this.EntitySearchOptions = getEndPointOptionsForEntity();
    if (this._addSFIService.sfiDetails) {
      this.setEntityDetails();
    }
    this.isViewMode = this._activatedRoute.snapshot.queryParamMap.get('mode') === 'view';
    this._addSFIService.previousURL = this._navigationService.previousURL ?
      this._navigationService.previousURL : '/fibi/dashboard/coi-list';
  }

  setEntityDetails() {
    this.entityDetails = JSON.parse(JSON.stringify(this._addSFIService.sfiDetails.coiFinancialEntity));
    this.entityDetails.involvementStartDate = getDateObjectFromTimeStamp(this.entityDetails.involvementStartDate);
    this.entityDetails.involvementEndDate = getDateObjectFromTimeStamp(this.entityDetails.involvementEndDate);
    this.countrySearchOptions.defaultValue = this.entityDetails.coiEntity.country ? this.entityDetails.coiEntity.country.countryName : '';
    this.EntitySearchOptions.defaultValue = this.entityDetails.coiEntity.coiEntityName;
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  setEntityTypeObj() {
    this.entityDetails.coiEntity.entityType = this.sfiLookUpList.entityType.find(ele =>
      this.entityDetails.coiEntity.entityTypeCode == ele.entityTypeCode);
  }

  selectedEvent(event) {
    if (event) {
      this.entityDetails.coiEntity = event;
      this.entityDetails.coiEntityId = event.coiEntityId;
      if (event.country) {
        this.countrySearchOptions.defaultValue = event.country.countryName;
        this.selectedCountryEvent(event.country);
      }
      this.clearCountryField = new String('false');
    } else {
      this.entityDetails.coiEntity = new CoiEntity();
      this.clearCountryField = new String('true');
      this.clearSFIFields();
    }
  }

  selectNewEntity(event) {
    this.entityDetails.coiEntity.coiEntityName = event.searchString;
  }

  selectedCountryEvent(event) {
    this.entityDetails.coiEntity.country = event;
    this.entityDetails.coiEntity.countryCode = event.countryCode;
  }

  saveSfi() {
    if (this.checkMandatoryFilled() && !this.isSaving) {
      this.isSaving = true;
      this.entityDetails.involvementStartDate = parseDateWithoutTimestamp(this.entityDetails.involvementStartDate);
      this.entityDetails.involvementEndDate = parseDateWithoutTimestamp(this.entityDetails.involvementEndDate);
      this.$subscriptions.push(this._addSFIService.createSfiDetails({ 'coiFinancialEntity': this.entityDetails,
            // tslint:disable-next-line: radix
            disclosureId: parseInt(this._activatedRoute.snapshot.queryParamMap.get('dId')),
            proposalDisclosureWithNoSfi : this._activatedRoute.snapshot.queryParamMap.get('isSFINotAvailable') == 'true' ? true : false
      })
        .subscribe((data: any) => {
          this._addSFIService.sfiDetails = JSON.parse(JSON.stringify(data));
          this.isSaving = false;
          this.isShowCollapse = false;
          this._router.navigate([], {
            queryParams: {
              entityId: data.coiFinancialEntity.coiFinancialEntityId
            },
            queryParamsHandling: 'merge',
          });
          this._commonService.showToast(HTTP_SUCCESS_STATUS, 'SFI created successfully');
          this.entityDetails = data.coiFinancialEntity;
          this.EntitySearchOptions.defaultValue = this.entityDetails.coiEntity.coiEntityName;
          this.entityDetails.involvementStartDate = getDateObjectFromTimeStamp(this.entityDetails.involvementStartDate);
          this.entityDetails.involvementEndDate = getDateObjectFromTimeStamp(this.entityDetails.involvementEndDate);
        },
          err => {
            this.isSaving = false;
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving SFI details. Please try again.');
          }));
    }
  }

  checkMandatoryFilled() {
    this.mandatoryList.clear();
    if (!this.entityDetails.coiEntity.coiEntityName) {
      this.mandatoryList.set('name', '* Please choose a entity name.');
    }
    if (!this.entityDetails.coiEntityId) {
      if (!this.entityDetails.coiEntity.entityTypeCode || this.entityDetails.coiEntity.entityTypeCode === 'null') {
        this.mandatoryList.set('type', '* Please choose a entity type.');
      }
      if (!this.entityDetails.coiEntity.country) {
        this.mandatoryList.set('country', '* Please choose a country.');
      }
      if (!this.entityDetails.coiEntity.pincode) {
        this.mandatoryList.set('pincode', '* Please enter a pincode.');
      }
      if (!this.entityDetails.coiEntity.webUrl) {
        this.mandatoryList.set('url', '* Please enter a weburl.');
      }
      if (!this.entityDetails.coiEntity.emailAddress) {
        this.mandatoryList.set('email', '* Please enter a email address.');
      }
      this.emailValidation();
    }
    if (!this.entityDetails.involvementStartDate) {
      this.mandatoryList.set('date', '* Please enter a start date.');
    }
    return this.mandatoryList.size !== 0 || this.emailWarningMsg ? false : true;
  }

  clearSFIFields() {
    this.mandatoryList.clear();
    this.entityDetails = new CoiFinancialEntity();
    this.clearCountryField = new String('true');
    this.clearField = new String('true');
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

  navigateBack() {
    this._router.navigateByUrl(this._addSFIService.previousURL);
  }

}
