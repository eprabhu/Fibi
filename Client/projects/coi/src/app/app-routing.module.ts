import {NgModule} from '@angular/core';
import {RouterModule, Routes} from '@angular/router';
import {AppRouterComponent} from "./common/app-router/app-router.component";

const routes: Routes = [
    {path: '', redirectTo: 'coi/disclosure', pathMatch: 'full'},
    {
        path: 'coi', component: AppRouterComponent, children: [
            {
                path: 'disclosure',
                loadChildren: () => import('./disclosure/disclosure.module').then(m => m.DisclosureModule)
            },
            {
                path: 'user-dashboard',
                loadChildren: () => import('./user-dashboard/user-dashboard.module').then(m => m.UserDashboardModule)
            },
            {
                path: 'admin-dashboard',
                loadChildren: () => import('./admin-dashboard/admin-dashboard.module').then(m => m.AdminDashboardModule)
            },
            {
                path: 'entity-management',
                loadChildren: () => import('./entity-management/entity-management.module').then(m => m.EntityManagementModule)
            },
        ]
    },
    {path: 'login', loadChildren: () => import('./login/login.module').then(m => m.LoginModule)}
];

@NgModule({
    imports: [RouterModule.forRoot(routes)],
    exports: [RouterModule]
})
export class AppRoutingModule {
}
