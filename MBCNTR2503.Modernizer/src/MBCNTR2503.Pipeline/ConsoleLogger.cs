using System;

namespace MBCNTR2503.Pipeline
{
    /// <summary>
    /// Simple console logger implementation
    /// </summary>
    public class ConsoleLogger : ILogger
    {
        public void LogInformation(string message)
        {
            Console.WriteLine($"[{DateTime.Now:HH:mm:ss}] INFO: {message}");
        }

        public void LogError(string message)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            Console.WriteLine($"[{DateTime.Now:HH:mm:ss}] ERROR: {message}");
            Console.ResetColor();
        }

        public void LogWarning(string message)
        {
            Console.ForegroundColor = ConsoleColor.Yellow;
            Console.WriteLine($"[{DateTime.Now:HH:mm:ss}] WARN: {message}");
            Console.ResetColor();
        }
    }
}