import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { ActivityService } from '../../../../../../fibi/src/app/agreement/agreement-shared/activity-track/activity.service';
import { slideHorizontal } from '../../../../../../fibi/src/app/common/utilities/animations';
import { SfiService } from '../../../disclosure/sfi/sfi.service';
import { environment } from '../../../../environments/environment';
import { CommonService } from '../../../common/services/common.service';
import { CoiEntity, CoiFinancialEntity } from '../add-sfi.interface';
import { getEndPointOptionsForCountry, getEndPointOptionsForEntity } from '../../../../../../fibi/src/app/common/services/end-point.config';

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
  animations: [slideHorizontal]
})
export class AddSfiComponent implements OnInit {

  isSaving = false;
  scrollHeight: number;
  deployMap = environment.deployUrl;
  @ViewChild('sfiNavOverlay', { static: true }) sfiNavOverlay: ElementRef;
  isAddAttachment = false;
  isAddAssignee = false;
  dateTime: string;
  isReadMore: false;
  showRelationshipModal = false;
  clearField: any = false;
  EntitySearchOptions: any = {};
  entityDetails: CoiFinancialEntity = new CoiFinancialEntity();
  countrySearchOptions: EndpointOptions;
  clearCountryField: any

  constructor(public sfiService: SfiService, public _commonService: CommonService) { }

  ngOnInit() {
    this.showSfiNavBar();
    this.EntitySearchOptions = getEndPointOptionsForEntity();
    this.countrySearchOptions = getEndPointOptionsForCountry();
  }

  hideSfiNavBar() {
    if (!this.isSaving) {
      this.sfiService.isShowSfiNavBar = false;
      this.showSfiNavBar();
    }
  }

  showSfiNavBar() {
    if (this.sfiService.isShowSfiNavBar) {
      this.sfiNavOverlay.nativeElement.style.display = 'block';
      this.scrollHeight = document.documentElement.scrollTop;
      document.body.style.overflow = 'hidden';
      document.documentElement.style.top = - this.scrollHeight + 'px';
    } else {
      this.sfiNavOverlay.nativeElement.style.display = 'none';
      document.body.style.overflow = 'auto';
      document.documentElement.scrollTop = this.scrollHeight;
    }
  }

  addEntityToggle(event) {
    this.showRelationshipModal = event;
  }

  saveOrUpdateCOIEntity() {
    const BODY = ''
    this.sfiService.saveOrUpdateCOIEntity(BODY).subscribe((res: any) => {
      if (res) {

      }
    },
      err => {

      });
  }

  selectNewEntity(event) {
    this.entityDetails.coiEntity.coiEntityName = event.searchString;
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

  selectedCountryEvent(event) {
    this.entityDetails.coiEntity.country = event;
    this.entityDetails.coiEntity.countryCode = event.countryCode;
  }
  clearSFIFields() {
    this.entityDetails = new CoiFinancialEntity();
    this.clearCountryField = new String('true');
    this.clearField = new String('true');
  }
}
