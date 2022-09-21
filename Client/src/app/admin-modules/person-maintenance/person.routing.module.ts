import { RouterModule } from '@angular/router';
import { NgModule } from '@angular/core';
import { PersonMaintenanceComponent } from './person-maintenance.component';
import { DegreeModule } from './degree/degree.module';

const routes = [{
  path: '', component: PersonMaintenanceComponent,
  children: [
    {path: 'person-details', loadChildren: () => import('../../admin-modules/person-maintenance/person-details/person-details.module')
    .then(m => m.PersonDetailsModule) },
    {path: 'orcid', loadChildren: () => import('../../admin-modules/person-maintenance/orcid/orcid.module').then(m => m.OrcidModule) },
    {path: 'delegation', loadChildren: () => import('../../admin-modules/person-maintenance/delegation/delegation.module').then(m => m.DelegationModule) },
    {path: 'timesheet', loadChildren: () => import('../../admin-modules/person-maintenance/timesheet/timesheet.module').then(m => m.TimesheetModule) },
    {path: 'training', loadChildren: () => import('../../admin-modules/person-maintenance/training/training.module').then(m => m.TrainingModule)},
    {path: 'degree', loadChildren: () => import('../../admin-modules/person-maintenance/degree/degree.module').then(m => m.DegreeModule)}
    ]
}];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class PersonRoutingModule { }
