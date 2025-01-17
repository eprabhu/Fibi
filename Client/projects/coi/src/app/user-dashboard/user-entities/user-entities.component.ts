import { Component, ElementRef, HostListener, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { RO } from "../../disclosure/coi-interface";
import { SfiService } from '../../disclosure/sfi/sfi.service';
import { UserEntitiesService } from "./user-entities.service";
import { CommonService } from '../../common/services/common.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_SUCCESS_STATUS, HTTP_ERROR_STATUS } from '../../app-constants';
import { Subject, interval } from 'rxjs';
import { debounce, switchMap } from 'rxjs/operators';
import { listAnimation, fadeInOutHeight, leftSlideInOut } from '../../common/utilities/animations';
import { hideModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { openCoiSlider } from '../../common/utilities/custom-utilities';

@Component({
  selector: 'app-user-entities',
  templateUrl: './user-entities.component.html',
  styleUrls: ['./user-entities.component.scss'],
  animations: [listAnimation, fadeInOutHeight, leftSlideInOut],
  providers: [UserEntitiesService]
})
export class UserEntitiesComponent implements OnInit, OnDestroy {
  sfiDashboardRequestObject = new RO();
  $subscriptions = [];
  entityArray = [];
  filteredEntityArray = [];
  searchText = '';
  result: any;
  showSlider = false;
  entityId: any;
  isEnableActivateInactivateSfiModal: boolean;
  entityName: any;
  personEntityId: any;
  personEntityNumber: any;
  updatedRelationshipStatus: String;
  $debounceEventForEntities = new Subject();
  $fetchSFI = new Subject();
  isSearchTextHover = false;
  isLoading = false;
  isHideFilterSearchAndShowCreate = false;
  isConcurrency = false;
  sliderElementId = '';
  relationshipDetails: any = {};
  entityDetails: any = {};

  constructor(private _userEntityService: UserEntitiesService, private _router: Router,
    private _sfiService: SfiService, private _commonService: CommonService, private elementRef: ElementRef) {
  }

  ngOnInit(): void {
    this.fetchMyEntities();
    this.$fetchSFI.next();
    this.getSearchList();
    window.scrollTo(0,0);
  }
  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

 fetchMyEntities() {
    this.sfiDashboardRequestObject.personId = this._commonService.getCurrentUserDetail('personID');
    this.$subscriptions.push(this.$fetchSFI.pipe(
      switchMap(() => {
        this.isLoading = true;
        return this._userEntityService.getSFIDashboard(this.sfiDashboardRequestObject)
      })).subscribe((data: any) => {
      this.result = data;
      if (this.result) {
        this.filteredEntityArray = data.personEntities || [];
        this.loadingComplete();
      }
    }), (err) => {
      this.loadingComplete();
      this.filteredEntityArray = [];
    });
  }

  private loadingComplete() {
    if (this.sfiDashboardRequestObject.filterType === 'ALL' && !this.searchText && this.sfiDashboardRequestObject.currentPage === 1) {
      this.isHideFilterSearchAndShowCreate = this.filteredEntityArray.length == 0 ? true : false;
    }
    this.isLoading = false;
}

  viewEntityDetails(entities) {

    this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: entities.coiFinancialEntityId, personEntityNumber: entities.personEntityNumber } })
  }

  setFilter(type = 'ALL') {
    this.searchText = '';
    this.filteredEntityArray = [];
    this.sfiDashboardRequestObject.filterType = type;
    this.sfiDashboardRequestObject.currentPage = 1;
    this.sfiDashboardRequestObject.searchWord = '';
    this.$fetchSFI.next();
  }

  listenForAdd() {
    this.$subscriptions.push(
        this._sfiService.$addSfi.subscribe((data: boolean) => {
          this.$fetchSFI.next();
          this._sfiService.isShowSfiNavBar = false;
            this.removeEntityId();
        })
    );
}

removeEntityId() {
    this._router.navigate([], {
      queryParams: {entityId: null},
      queryParamsHandling: 'merge'
    })
  }

  getRelationshipTypes(relationshipTypes) {
    if(relationshipTypes) {
      return relationshipTypes.split(',').map((type: any) => {
        const lowercase = type.toLowerCase();
        return ' ' + lowercase.charAt(0).toUpperCase() + lowercase.slice(1);
      }).join(',');
    }
  }

  actionsOnPageChangeEvent(event) {
    if (this.sfiDashboardRequestObject.currentPage != event) {
      this.sfiDashboardRequestObject.currentPage = event;
      this.$fetchSFI.next();
    }
  }

  activateDeactivateEvent(event) {
    this.entityDetails = event.coiEntity;
    this.relationshipDetails = event.validPersonEntityRelTypes;
    this.isEnableActivateInactivateSfiModal = true;
    this.personEntityId = event.personEntityId;
    this.entityName = event.coiEntity.entityName;
    this.updatedRelationshipStatus = event.versionStatus === 'ACTIVE' ? 'INACTIVE' : 'ACTIVE';
    this.personEntityNumber = event.personEntityNumber;
  }

  deleteSFIConfirmation(event) {
    this.personEntityId = event.eId;
    this.entityName = this.filteredEntityArray.find(ele => ele.personEntityId === this.personEntityId).coiEntity.entityName;
    document.getElementById('hidden-delete-button').click();
  }

  viewSlider(event) {
    this.showSlider = event.flag;
    this.entityId = event.entityId;
    this.sliderElementId = `admin-dashboard-entity-slider-${this.entityId}`;
    openCoiSlider(this.sliderElementId);
  }

  getEntities() {
    this.sfiDashboardRequestObject.currentPage = 1;
    this.$debounceEventForEntities.next('');
  }

  getSearchList() {
    this.$subscriptions.push(this.$debounceEventForEntities.pipe(debounce(() => interval(800))).subscribe((data: any) => {
      this.sfiDashboardRequestObject.searchWord = this.searchText;
      this.$fetchSFI.next();
    }
    ));
  }

  hideSfiNavBar() {
    setTimeout(() => {
        this.showSlider = false;
        this.entityId = null;
        this.sliderElementId = '';
    },500);
}

closeActivateInactivateSfiModal(event) {
  this.isEnableActivateInactivateSfiModal = false;
  if(event) {
    this.$fetchSFI.next();
  }
}

deleteSFI() {
  this._sfiService.deleteSFI(this.personEntityId).subscribe((data:any) => {
      this.sfiDashboardRequestObject.currentPage = 1;
      this.$fetchSFI.next();
      this._commonService.showToast(HTTP_SUCCESS_STATUS, 'SFI deleted successfully.');
  }, err=> {
    if (err.status === 405) {
      hideModal('deleteSFIConfirmationModal');
      this.isConcurrency = true;
    } else {
      this._commonService.showToast(HTTP_ERROR_STATUS, 'SFI deletion canceled.');
    }
  })
}

clearSearchText() {
  this.searchText = '';
  this.sfiDashboardRequestObject.searchWord = '';
  this.$fetchSFI.next();
}


addSFI(type) {
  this._router.navigate(['/coi/create-sfi/create'], { queryParams: { type: 'SFI' } });
}

}
