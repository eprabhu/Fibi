import { ChangeDetectionStrategy, Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { easeIn } from '../../../../../../fibi/src/app/common/utilities/animations';

@Component({
  selector: 'app-define-relation',
  templateUrl: './define-relation.component.html',
  styleUrls: ['./define-relation.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  animations: [easeIn],
})
export class DefineRelationComponent implements OnInit {

  @Input() ruleIndex: number;
  @Output() closePage: EventEmitter<any> = new EventEmitter<any>();

  isMaximized: boolean = false;

  isProjectListVisible = true;
  coiStatusList: any = [];
  selectedProject: any;
  // $subscriptions: Subscription[] = [];
  coiStatusCode: any;
  coiDescription: any;
  sfiDetails: any;
  dependencies = ['coiDisclosure'];
  currentProjectId: any;
	isOpenModal = false;

  newArray: Array<any> = [];
  disclosureNumber: any;
  disclosureStatusCode: any;
  isEditMode: boolean = true;
  personId: any;
  isShowInfo = true;
  coiDisclosure: any;

  constructor() { }

  ngOnInit() {
    // this.loadProjectRelations();
    this.createNewArray();
    // this.maximizeTree();
  }

  createNewArray() {
    this.newArray.push({'coiEntityName': 'Google', 'discDetStatusCode': 1, 'comment': {'comments': 'test comment'}});
    this.newArray.push({'coiEntityName': 'Alexon Pharmaceuticals', 'discDetStatusCode': 1, 'comment': {'comments': 'test comment'}});
    this.newArray.push({'coiEntityName': 'Daliki Sankiyo', 'discDetStatusCode': 1, 'comment': {'comments': 'test comment'}});
    this.newArray.push({'coiEntityName': 'Elisa', 'discDetStatusCode': 1, 'comment': {'comments': 'test comment'}});
    this.newArray.push({'coiEntityName': 'Google', 'discDetStatusCode': 1, 'comment': {'comments': 'test comment'}});
    this.newArray.push({'coiEntityName': 'Alexon Pharmaceuticals', 'discDetStatusCode': 1, 'comment': {'comments': 'test comment'}});
    this.newArray.push({'coiEntityName': 'Daliki Sankiyo', 'discDetStatusCode': 1, 'comment': {'comments': 'test comment'}});
    this.newArray.push({'coiEntityName': 'Elisa', 'discDetStatusCode': 1, 'comment': {'comments': 'test comment'}});
    this.newArray.push({'coiEntityName': 'Google', 'discDetStatusCode': 1, 'comment': {'comments': 'test comment'}});
    this.newArray.push({'coiEntityName': 'Alexon Pharmaceuticals', 'discDetStatusCode': 1, 'comment': {'comments': 'test comment'}});
    this.newArray.push({'coiEntityName': 'Daliki Sankiyo', 'discDetStatusCode': 1, 'comment': {'comments': 'test comment'}});
    this.newArray.push({'coiEntityName': 'Elisa', 'discDetStatusCode': 1, 'comment': {'comments': 'test comment'}});
    this.selectedProject = {
      principalInvestigator: 'Smith, Will',
      sponsor: 'Air Force',
      startDate: '27/02/2012',
      endDate: '27/02/2022',
      primeSponsor: 'Aerospace Nation',
      moduleStatus: 'In Progress',
      leadUnit: '100000 - RMIT',
    }
    this.coiStatusList = [{'description':'No Conflict', 'discDetStatusCode': '1'},
    {'description':'Potential Conflict', 'discDetStatusCode': '2'},
    {'description':'Conflict Identified', 'discDetStatusCode': '3'}]
  }

    /**
   * updates the width of the tree to maximum according to the size of the current screen
  // */
  //   maximizeTree() {
  //     this.isMaximized = true;
  //     setTimeout(() => {
  //       (document.getElementsByClassName('qst-tree-content')[0] as HTMLElement).style.width = window.innerWidth - 240 + 'px';
  //       (document.getElementsByClassName('show-chat')[0] as HTMLElement).style.width = window.innerWidth - 240 + 'px';
  //     });
  //   }
  //   /** updates the width of the tree to 192px which fits into the left size of the screen
  //   */
    minimizeTree() {
      // this.isMaximized = false;
      // (document.getElementsByClassName('qst-tree-content')[0] as HTMLElement).style.width = '0';
      // (document.getElementsByClassName('show-chat')[0] as HTMLElement).style.width = '0';
      // setTimeout(() => {
        this.closePage.emit(false);
      // });
    }

    applyToAll() {
      this.newArray.forEach((ele: any) => {
        if (!ele.discDetStatusCode) {
          ele.discDetStatusCode = this.coiStatusCode;
        } if (!ele.comment.comments) {
          ele.comment.comments = this.coiDescription;
        }
      });
    }

    // loadProjectRelations() {
    //   this._relationShipService.getProjectRelations().subscribe((data: any) => {
    //     this.coiStatusList = data.coiDisclosureDetailStatuses;
    //   });
    // }

}

