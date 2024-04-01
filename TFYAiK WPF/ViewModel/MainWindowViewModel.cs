using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TFYAiK_WPF.ViewModel.Base;

namespace TFYAiK_WPF.ViewModel
{
    class MainWindowViewModel : ViewModel.Base.ViewModel
    {
        #region Заголовок окна
        private string _Title = "PASCAL: логические выражения";
        public string Title
        {
            get => _Title;
            set => Set(ref _Title, value);
        }
#endregion
    }
}
