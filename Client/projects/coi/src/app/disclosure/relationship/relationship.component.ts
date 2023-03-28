import { Component } from '@angular/core';

@Component({
  selector: 'app-relationship',
  templateUrl: './relationship.component.html',
  styleUrls: ['./relationship.component.scss']
})
export class RelationshipComponent {
  isShowRelation = false;
  proposalArray = [];
  closePage() {
    this.isShowRelation = false;
  }

  ngOnInit() {
    let selectedProject = {
      title: '#11111-Enginnering of sustainable and adaptable 3D-printed material',
      principalInvestigator: 'Smith, Will',
      sponsor: 'Air Force',
      startDate: '27/02/2012',
      endDate: '27/02/2022',
      primeSponsor: 'Aerospace Nation',
      moduleStatus: 'In Progress',
      leadUnit: '100000 - RMIT',
    }
    this.proposalArray.push(selectedProject);
    this.proposalArray.push(selectedProject);
    this.proposalArray.push(selectedProject);
  }
}
